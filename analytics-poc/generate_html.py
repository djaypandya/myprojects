import markdown
import os
import base64

REPORT_FILE = 'storytelling_report.md'
HTML_FILE = 'Fraud_Analytics_Report.html'
ASSETS_DIR = 'assets'

def image_to_base64(image_path):
    if not os.path.exists(image_path):
        return ""
    with open(image_path, "rb") as img_file:
        return base64.b64encode(img_file.read()).decode('utf-8')

def convert():
    if not os.path.exists(REPORT_FILE):
        print(f"Error: {REPORT_FILE} not found.")
        return

    print(f"Reading {REPORT_FILE}...")
    with open(REPORT_FILE, 'r') as f:
        md_text = f.read()

    # Replace image links with base64
    # Markdown format: ![Alt](path)
    # We'll do a simple replace for the known assets for robustness
    known_assets = [
        'assets/1_exception_funnel.png',
        'assets/2_amount_risk.png',
        'assets/3_velocity_burst.png',
        'assets/4_ml_scores.png'
    ]
    
    for asset in known_assets:
        if asset in md_text:
            b64 = image_to_base64(asset)
            if b64:
                # Replace with HTML img tag with base64
                img_tag = f'<img src="data:image/png;base64,{b64}" alt="Chart">'
                # Regex or simple replace? Simple replace is safer if exact match
                # The markdown has ![...](assets/...)
                # Let's find the markdown image syntax
                import re
                pattern = rf'!\[.*?\]\({re.escape(asset)}\)'
                md_text = re.sub(pattern, img_tag, md_text)

    # Convert to HTML
    html_content = markdown.markdown(md_text, extensions=['tables', 'fenced_code'])

    # CSS for A4 PDF-like look
    css = """
    <style>
        @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap');
        body { 
            font-family: 'Roboto', sans-serif; 
            line-height: 1.6; 
            color: #333;
            max-width: 210mm; /* A4 width */
            margin: 0 auto;
            padding: 20px;
            background: #fff;
        }
        @media print {
            body { max-width: 100%; margin: 0; padding: 0; }
            .no-print { display: none; }
        }
        h1 { color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px; margin-top: 0; }
        h2 { color: #e67e22; margin-top: 30px; border-left: 5px solid #e67e22; padding-left: 10px; }
        h3 { color: #34495e; margin-top: 20px; }
        img { max-width: 100%; height: auto; margin: 20px 0; border: 1px solid #eee; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; font-size: 0.9em; }
        th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }
        th { background-color: #f8f9fa; font-weight: bold; }
        code { background-color: #f4f4f4; padding: 2px 5px; border-radius: 3px; font-family: monospace; }
        .header-banner {
            background: #2c3e50;
            color: white;
            padding: 20px;
            margin-bottom: 30px;
            border-radius: 5px;
        }
        .print-btn {
            background: #e67e22;
            color: white;
            border: none;
            padding: 10px 20px;
            border-radius: 5px;
            cursor: pointer;
            font-size: 16px;
            margin-bottom: 20px;
        }
        .print-btn:hover { background: #d35400; }
    </style>
    """
    
    # Add Print Button
    print_btn = """
    <div class="no-print" style="text-align: right;">
        <button class="print-btn" onclick="window.print()">🖨️ Save as PDF</button>
    </div>
    """
    
    full_html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <title>Fraud Analytics Report</title>
        {css}
    </head>
    <body>
        {print_btn}
        {html_content}
    </body>
    </html>
    """

    print(f"Generating {HTML_FILE}...")
    with open(HTML_FILE, "w") as f:
        f.write(full_html)
    
    print(f"Success! Report saved to {os.path.abspath(HTML_FILE)}")

if __name__ == "__main__":
    convert()
