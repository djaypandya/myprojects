"""
Complaint Detection Module

This module provides tools to analyse customer call notes and detect potential complaints.
It offers two methods:
1. NLTK-based classifier: Uses sentiment analysis and heuristics.
2. Keyword-based scorer: Uses a user-provided dictionary of keywords and scores.

It also provides a utility to compare the results of these two methods.
"""

import pandas as pd
import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import string

def _check_nltk_resources():
    """
    Checks for required NLTK resources and downloads them if missing.
    """
    # Workaround for SSL certificate issues on some systems (e.g. macOS)
    import ssl
    try:
        _create_unverified_https_context = ssl._create_unverified_context
    except AttributeError:
        pass
    else:
        ssl._create_default_https_context = _create_unverified_https_context

    resources = ['vader_lexicon', 'punkt', 'stopwords', 'punkt_tab']
    for res in resources:
        try:
            nltk.data.find(f'sentiment/{res}.zip') if res == 'vader_lexicon' else nltk.data.find(f'tokenizers/{res}.zip') if 'punkt' in res else nltk.data.find(f'corpora/{res}.zip')
        except LookupError:
            try:
                nltk.download(res, quiet=True)
            except Exception as e:
                print(f"Warning: Could not download nltk resource '{res}': {e}")

def preprocess_text(text, stopwords=None):
    """
    Preprocesses text for analysis.
    
    Args:
        text (str): The input text.
        stopwords (set, optional): A set of stopwords to remove.
        
    Returns:
        list: A list of lowercased tokens.
    """
    if not isinstance(text, str):
        return []
    
    # Lowercase
    text = text.lower()
    
    # Tokenize
    try:
        tokens = nltk.word_tokenize(text)
    except LookupError:
        _check_nltk_resources()
        tokens = nltk.word_tokenize(text)
        
    # Remove punctuation
    tokens = [t for t in tokens if t not in string.punctuation]
    
    # Remove stopwords if provided
    if stopwords:
        tokens = [t for t in tokens if t not in stopwords]
        
    return tokens

def detect_complaints_nltk(df, text_col, id_col, config=None):
    """
    Detects complaints using NLTK VADER sentiment analysis and heuristics.
    
    Args:
        df (pd.DataFrame): Input DataFrame.
        text_col (str): Name of the column containing text.
        id_col (str): Name of the ID column.
        config (dict, optional): Configuration dictionary. Can contain:
            - 'complaint_threshold': float (default 0.5), threshold for is_complaint
            - 'negative_weight': float (default 1.0), weight for negative sentiment
    
    Returns:
        pd.DataFrame: DataFrame with [id_col, text_col, 'is_complaint_nltk', 'complaint_strength_nltk']
    """
    _check_nltk_resources()
    sia = SentimentIntensityAnalyzer()
    
    cfg = config or {}
    threshold = cfg.get('complaint_threshold', 0.5)
    
    results = []
    
    for _, row in df.iterrows():
        text = row[text_col]
        if not isinstance(text, str):
            results.append({
                id_col: row[id_col],
                text_col: text,
                'is_complaint_nltk': False,
                'complaint_strength_nltk': 0.0
            })
            continue
            
        # Sentiment score (compound is -1 to 1, we want 0 to 1 scale roughly for "complaintness")
        # High negative sentiment -> likely complaint.
        # VADER compound: -1 (most neg) to +1 (most pos).
        # We map -1 to 1 (high complaint) and +1 to 0 (low complaint).
        scores = sia.polarity_scores(text)
        compound = scores['compound']
        
        # Invert compound: -1 becomes 1, 1 becomes 0. 
        # Formula: (1 - compound) / 2  ->  (-1 -> 1, 1 -> 0, 0 -> 0.5)
        # But complaints are specifically NEGATIVE. Neutral (0) isn't necessarily a complaint.
        # So let's just take negative component directly? 
        # Actually, let's stick to the inverted compound as a general "negativity/distress" score 
        # but maybe bias it.
        # Let's use the 'neg' score from VADER which is 0-1 proportion of text that is negative.
        # And maybe boost it if compound is very negative.
        
        # Simple heuristic: Complaint Strength = VADER 'neg' score + (1 if compound < -0.5 else 0) * 0.2
        # This is a heuristic.
        strength = scores['neg']
        if compound < -0.4:
            strength += 0.3 # Boost for overall negative tone
        
        # Cap at 1.0
        strength = min(1.0, strength)
        
        is_complaint = strength >= threshold
        
        results.append({
            id_col: row[id_col],
            text_col: text,
            'is_complaint_nltk': is_complaint,
            'complaint_strength_nltk': round(strength, 3)
        })
        
    return pd.DataFrame(results)

def score_keywords(df, text_col, id_col, keyword_dict):
    """
    Scores text based on a keyword dictionary.
    
    Args:
        df (pd.DataFrame): Input DataFrame.
        text_col (str): Name of the column containing text.
        id_col (str): Name of the ID column.
        keyword_dict (dict): Dictionary mapping keywords (str) to scores (numeric).
                             Keywords are matched case-insensitively.
    
    Returns:
        pd.DataFrame: DataFrame with [id_col, 'keyword_score']
    """
    results = []
    
    # Pre-process dictionary for faster lookup (lowercase keys)
    processed_dict = {k.lower(): v for k, v in keyword_dict.items()}
    
    for _, row in df.iterrows():
        text = row[text_col]
        score = 0
        
        if isinstance(text, str):
            text_lower = text.lower()
            # Simple substring check or token check? 
            # Requirement: "performs a binary check per keyword"
            # Let's do substring check for flexibility, but users might want token matching.
            # Given "call notes", phrases are common. Substring is safer for phrases.
            
            for kw, val in processed_dict.items():
                # Binary check: present at least once
                if kw in text_lower:
                    score += val
                    
        results.append({
            id_col: row[id_col],
            'keyword_score': score
        })
        
    return pd.DataFrame(results)

def compare_results(nltk_df, keyword_df, id_col):
    """
    Merges NLTK and Keyword results for comparison.
    
    Args:
        nltk_df (pd.DataFrame): Output from detect_complaints_nltk.
        keyword_df (pd.DataFrame): Output from score_keywords.
        id_col (str): The ID column name to join on.
        
    Returns:
        pd.DataFrame: Combined DataFrame.
    """
    # Ensure IDs are same type if possible, but pandas merge handles a lot.
    merged = pd.merge(nltk_df, keyword_df, on=id_col, how='outer')
    
    # Fill NaNs if any (e.g. if inputs had different rows, though unlikely in this workflow)
    merged['keyword_score'] = merged['keyword_score'].fillna(0)
    
    return merged

# ==========================================
# Extension: Multi-Dictionary Comparison
# ==========================================

def keyword_match_details_for_texts(df, text_col, keyword_dict):
    """
    Scores text and identifies matched keywords for a single dictionary.
    Optimized for memory usage by avoiding iterrows and DataFrame overhead per row.
    
    Args:
        df (pd.DataFrame): Input DataFrame.
        text_col (str): Name of the column containing text.
        keyword_dict (dict): Dictionary mapping keywords (str) to scores (numeric).
    
    Returns:
        pd.DataFrame: DataFrame with ['score', 'matched_keywords'] columns, 
                      aligned with input df index.
    """
    processed_dict = {k.lower(): v for k, v in keyword_dict.items()}
    
    # Use list comprehension for speed and lower memory overhead than iterrows
    # We iterate over the actual values, handling potential non-string values
    texts = df[text_col].values
    
    scores = []
    matched_keywords_list = []
    
    for text in texts:
        score = 0
        matches = []
        
        if isinstance(text, str):
            text_lower = text.lower()
            for kw, val in processed_dict.items():
                if kw in text_lower:
                    score += val
                    matches.append(kw)
        
        scores.append(score)
        matched_keywords_list.append(", ".join(sorted(matches)))
        
    # Create DataFrame directly from lists, indexed by original DF index
    return pd.DataFrame({
        'score': scores,
        'matched_keywords': matched_keywords_list
    }, index=df.index)

def apply_multiple_keyword_dictionaries(df, text_col, id_col, date_col, keyword_dicts, complaint_threshold=5.0):
    """
    Applies multiple keyword dictionaries to the data.
    
    Args:
        df (pd.DataFrame): Input DataFrame.
        text_col (str): Text column name.
        id_col (str): ID column name.
        date_col (str): Date column name.
        keyword_dicts (list or dict): Ordered list of keyword dictionaries. 
                                      If dict, keys are list names. If list, names are generated.
        complaint_threshold (float): Score threshold for flagging complaints.
        
    Returns:
        pd.DataFrame: Detailed DataFrame with per-list scores and flags, merged with original data.
    """
    # Start with a copy of the original dataframe to preserve all columns
    final_df = df.copy()
    
    # Handle input format for keyword_dicts
    if isinstance(keyword_dicts, list):
        dict_items = [(f"list_{i+1}", d) for i, d in enumerate(keyword_dicts)]
    elif isinstance(keyword_dicts, dict):
        dict_items = list(keyword_dicts.items())
    else:
        raise ValueError("keyword_dicts must be a list or dictionary")
        
    # Validation: Ensure id_col exists
    if id_col not in df.columns:
        raise ValueError(f"ID column '{id_col}' not found in DataFrame.")

    for name, k_dict in dict_items:
        # Get scores and matches (returns DF aligned with final_df index)
        res = keyword_match_details_for_texts(final_df, text_col, k_dict)
        
        # Assign directly to columns - much faster and memory efficient than merge
        final_df[f'score_{name}'] = res['score']
        final_df[f'matched_keywords_{name}'] = res['matched_keywords']
        
        # Calculate complaint flag
        final_df[f'is_complaint_{name}'] = final_df[f'score_{name}'] >= complaint_threshold

    return final_df

def compute_keyword_list_deltas(detailed_df, list_names, id_col):
    """
    Computes summary statistics and deltas between consecutive lists.
    
    Args:
        detailed_df (pd.DataFrame): Output from apply_multiple_keyword_dictionaries.
        list_names (list): List of names used in the detailed_df (e.g. ['list_1', 'list_2']).
        id_col (str): Name of the ID column to identify flagged records.
        
    Returns:
        dict: Summary information including counts and deltas.
    """
    summary = {
        'lists': {},
        'deltas': []
    }
    
    # Per-list summaries
    for name in list_names:
        col = f'is_complaint_{name}'
        if col in detailed_df.columns:
            count = detailed_df[col].sum()
            summary['lists'][name] = int(count)
            
    # Consecutive pair deltas
    for i in range(len(list_names) - 1):
        curr = list_names[i]
        next_l = list_names[i+1]
        
        col_curr = f'is_complaint_{curr}'
        col_next = f'is_complaint_{next_l}'
        
        if col_curr in detailed_df.columns and col_next in detailed_df.columns:
            # Additional: Flagged in NEXT but NOT in CURR
            additional_mask = (~detailed_df[col_curr]) & (detailed_df[col_next])
            
            # Use id_col to extract IDs
            if id_col in detailed_df.columns:
                additional_ids = detailed_df.loc[additional_mask, id_col].tolist()
            else:
                # Fallback if id_col missing from detailed_df (shouldn't happen if passed correctly)
                additional_ids = detailed_df.loc[additional_mask].index.tolist()
            
            summary['deltas'].append({
                'pair': f"{curr} -> {next_l}",
                'count_additional': int(additional_mask.sum()),
                'additional_ids': additional_ids
            })
            
    return summary

def build_html_keyword_comparison_report(summary_info):
    """
    Generates an HTML summary report.
    
    Args:
        summary_info (dict): Output from compute_keyword_list_deltas.
        
    Returns:
        str: HTML string.
    """
    html = ["<html><body>"]
    html.append("<h1>Keyword Dictionary Comparison Report</h1>")
    
    # List Summaries
    html.append("<h2>Complaint Counts per Dictionary</h2>")
    html.append("<table border='1' cellpadding='5' style='border-collapse: collapse;'>")
    html.append("<tr><th>Dictionary</th><th>Flagged Complaints</th></tr>")
    for name, count in summary_info['lists'].items():
        html.append(f"<tr><td>{name}</td><td>{count}</td></tr>")
    html.append("</table>")
    
    # Deltas
    html.append("<h2>Progression (Additional Flags)</h2>")
    html.append("<table border='1' cellpadding='5' style='border-collapse: collapse;'>")
    html.append("<tr><th>Transition</th><th>Additional Complaints</th><th>IDs (Sample)</th></tr>")
    for delta in summary_info['deltas']:
        ids_str = ", ".join(map(str, delta['additional_ids'][:10])) # Sample first 10
        if len(delta['additional_ids']) > 10:
            ids_str += "..."
        html.append(f"<tr><td>{delta['pair']}</td><td>{delta['count_additional']}</td><td>{ids_str}</td></tr>")
    html.append("</table>")
    
    html.append("</body></html>")
    return "\n".join(html)

def generate_keyword_comparison_report(df, text_col, id_col, date_col, keyword_dicts, complaint_threshold=5.0):
    """
    Orchestrates the multi-dictionary comparison and report generation.
    
    Args:
        df (pd.DataFrame): Input DataFrame.
        text_col (str): Text column name.
        id_col (str): ID column name.
        date_col (str): Date column name.
        keyword_dicts (list): Ordered list of 4 keyword dictionaries.
        complaint_threshold (float): Threshold for flagging.
        
    Returns:
        tuple: (detailed_df, summary_info, html_report)
    """
    # 1. Apply dictionaries
    detailed_df = apply_multiple_keyword_dictionaries(df, text_col, id_col, date_col, keyword_dicts, complaint_threshold)
    
    # 2. Compute deltas
    # Generate list names based on input
    list_names = [f"list_{i+1}" for i in range(len(keyword_dicts))]
    summary_info = compute_keyword_list_deltas(detailed_df, list_names, id_col)
    
    # 3. Build HTML
    html_report = build_html_keyword_comparison_report(summary_info)
    
    return detailed_df, summary_info, html_report

# ==========================================
# Extension: Excel Export & Reporting
# ==========================================

def build_keyword_reference_table(keyword_dicts):
    """
    Flattens a list of keyword dictionaries into a single DataFrame.
    
    Args:
        keyword_dicts (list): Ordered list of keyword dictionaries.
        
    Returns:
        pd.DataFrame: Table with columns [keyword, score, list_name].
    """
    rows = []
    for i, k_dict in enumerate(keyword_dicts):
        list_name = f"list_{i+1}"
        for kw, score in k_dict.items():
            rows.append({
                'keyword': kw,
                'score': score,
                'list_name': list_name
            })
    return pd.DataFrame(rows)

def compute_export_metrics(original_df, detailed_df, summary_info, keyword_dicts):
    """
    Computes metrics for the narrative summary.
    
    Args:
        original_df (pd.DataFrame): Original input data.
        detailed_df (pd.DataFrame): Processed data with complaint flags.
        summary_info (dict): Summary info from comparison phase.
        keyword_dicts (list): List of keyword dictionaries.
        
    Returns:
        dict: Dictionary of metrics.
    """
    metrics = {
        'total_rows_original': len(original_df),
        'complaint_counts': {},
        'deltas': summary_info.get('deltas', [])
    }
    
    # Get per-list counts from detailed_df to be sure
    for i in range(len(keyword_dicts)):
        list_name = f"list_{i+1}"
        col = f"is_complaint_{list_name}"
        if col in detailed_df.columns:
            metrics['complaint_counts'][list_name] = int(detailed_df[col].sum())
        else:
            metrics['complaint_counts'][list_name] = 0
            
    return metrics

def build_plain_english_summary(metrics):
    """
    Generates a plain English narrative summary of the analysis.
    
    Args:
        metrics (dict): Metrics computed by compute_export_metrics.
        
    Returns:
        str: Narrative summary text.
    """
    total_rows = metrics['total_rows_original']
    counts = metrics['complaint_counts']
    deltas = metrics['deltas']
    
    # Part 1: Context
    lines = []
    lines.append(f"We started with a dataset of {total_rows} customer notes.")
    lines.append("The goal was to identify potential complaints that might have been missed.")
    lines.append("We used four different lists of keywords to flag these notes, ranging from a basic list to more comprehensive ones.")
    lines.append("")
    
    # Part 2: Findings per list
    lines.append("Here is what we found:")
    for list_name, count in counts.items():
        # Clean up list name for display (e.g. list_1 -> List 1)
        display_name = list_name.replace('_', ' ').title()
        lines.append(f"- {display_name} found {count} potential complaints.")
    lines.append("")
    
    # Part 3: Progression/Deltas
    if deltas:
        lines.append("Comparing the lists showed how many new complaints were found as we added more keywords:")
        for delta in deltas:
            pair = delta['pair'].replace('list_', 'List ')
            added = delta['count_additional']
            lines.append(f"- Moving from {pair} identified {added} additional potential complaints.")
            
    # Part 4: High-level pattern (simple heuristic)
    # Check if returns are diminishing
    if len(deltas) >= 2:
        first_jump = deltas[0]['count_additional']
        last_jump = deltas[-1]['count_additional']
        
        lines.append("")
        if last_jump < first_jump:
            lines.append("Overall, the earlier lists captured the bulk of the complaints. Adding more keywords later yielded fewer new results.")
        elif last_jump > first_jump:
            lines.append("Interestingly, the later, more specific keyword lists found a significant number of new complaints that the earlier lists missed.")
        else:
            lines.append("The number of new complaints found remained consistent as we expanded the keyword lists.")

    return "\n".join(lines)

def export_analysis_to_excel(original_df, detailed_df, keyword_dicts, summary_info, output_path):
    """
    Exports the analysis results to a multi-sheet Excel file.
    
    Args:
        original_df (pd.DataFrame): Original input data.
        detailed_df (pd.DataFrame): Processed data.
        keyword_dicts (list): List of keyword dictionaries.
        summary_info (dict): Summary info from comparison phase.
        output_path (str): Path to save the Excel file.
    """
    # 1. Prepare DataFrames
    
    # Sheet 1: Original Data
    # (Just original_df)
    
    # Sheet 2: Potential Complaints
    # Filter detailed_df for rows where ANY is_complaint_* column is True
    complaint_cols = [c for c in detailed_df.columns if c.startswith('is_complaint')]
    if complaint_cols:
        mask = detailed_df[complaint_cols].any(axis=1)
        potential_complaints_df = detailed_df[mask]
    else:
        potential_complaints_df = pd.DataFrame() # Empty if no columns found
        
    # Sheet 3: Keywords
    keywords_df = build_keyword_reference_table(keyword_dicts)
    
    # Sheet 4: Summary
    # Consolidate summary_info and counts into a nice table
    # We'll make a list of records
    summary_rows = []
    
    # General stats
    summary_rows.append({'Metric': 'Total Original Rows', 'Value': len(original_df), 'Details': ''})
    
    # Per-list counts
    for name, count in summary_info.get('lists', {}).items():
        summary_rows.append({'Metric': f'Complaints Found by {name}', 'Value': count, 'Details': ''})
        
    # Deltas
    for delta in summary_info.get('deltas', []):
        summary_rows.append({
            'Metric': f"Additional in {delta['pair']}", 
            'Value': delta['count_additional'], 
            'Details': f"IDs: {delta['additional_ids'][:5]}..." if delta['additional_ids'] else ""
        })
        
    summary_df = pd.DataFrame(summary_rows)
    
    # Sheet 5: Narrative Summary
    metrics = compute_export_metrics(original_df, detailed_df, summary_info, keyword_dicts)
    narrative_text = build_plain_english_summary(metrics)
    narrative_df = pd.DataFrame({'narrative': [narrative_text]})
    
    # 2. Write to Excel
    # Use 'w' mode (default) which creates a new file
    with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
        original_df.to_excel(writer, sheet_name='original_data', index=False)
        potential_complaints_df.to_excel(writer, sheet_name='potential_complaints', index=False)
        keywords_df.to_excel(writer, sheet_name='keywords', index=False)
        summary_df.to_excel(writer, sheet_name='summary', index=False)
        narrative_df.to_excel(writer, sheet_name='narrative_summary', index=False)
