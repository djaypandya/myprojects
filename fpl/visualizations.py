import plotly.graph_objects as go
import pandas as pd
import config

def create_league_animation_chart(df, highlight_config, metric='Rank'):
    """
    Creates an animated Plotly chart showing the progression of league positions or points.

    Args:
        df: DataFrame with columns ['GW', 'Entry ID', 'Name', 'Total Points', 'Rank', 'Delta']
        highlight_config: Dict mapping {entry_id: hex_color_string} for highlighted managers.
        metric: 'Rank' or 'Gap to Leader'

    Returns:
        go.Figure: The plotly figure object.
    """
    max_gw = df['GW'].max()
    all_entries = df['Entry ID'].unique()
    
    y_col = 'Rank' if metric == 'Rank' else 'Delta'
    hover_y_label = 'Rank: %{y}' if metric == 'Rank' else 'Gap: %{y}'
    
    # Determine Y-Axis Range and Autorange
    if metric == 'Rank':
        # Rank 1 is top (reversed)
        y_autorange = "reversed" 
        # Fixed range: [Max Rank + 1, 0]
        max_val = df['Rank'].max()
        y_range = [max_val + 1, 0]
        title_text = "League Positions (Animation)"
    else:
        # Gap to Leader: 0 is best (Top). Higher delta is worse.
        # So we also REVERSE this axis.
        y_autorange = "reversed" 
        # Fixed range: [Max Delta * 1.05, -5 (buffer for leader)]
        max_val = df['Delta'].max()
        # If max_val is 0 (all tied?), handle gracefully
        upper_bound = max_val * 1.05 if max_val > 0 else 10
        y_range = [upper_bound, -2] # -2 allows 0 to be clearly visible below top
        title_text = "Gap to Leader (Points Behind)"

    fig = go.Figure()

    # --- Create Traces (Initial State: Empty) ---
    for eid in all_entries:
        # Get Name for Legend/Hover
        entry_rows = df[df['Entry ID'] == eid]
        entry_name = entry_rows['Name'].iloc[0] if not entry_rows.empty else str(eid)
        
        is_highlighted = eid in highlight_config
        custom_color = highlight_config.get(eid, "#808080")
        
        opacity = 1.0 if is_highlighted else 0.3
        width = 4 if is_highlighted else 1
        
        # Line Trace
        fig.add_trace(go.Scatter(
            x=[], y=[], 
            mode='lines+markers',
            name=entry_name,
            line=dict(color=custom_color, width=width),
            opacity=opacity,
            hoverinfo='name+x+y' if not is_highlighted else 'skip', 
            showlegend=is_highlighted # Show legend if highlighted
        ))
        
        # Highlight Dot Trace
        marker_size = 12 if is_highlighted else 6
        
        # For specific hover on the head of the line
        hovertemplate = None
        if is_highlighted:
            hovertemplate = (
                f"<b>{entry_name}</b><br>" +
                "GW: %{x}<br>" +
                f"{metric}: %{{y}}<br>" + # Dynamic Label
                "Pts: %{customdata[0]}<br>" +
                "Delta: %{customdata[1]}<extra></extra>"
            )
        else:
            hovertemplate = f"<b>{entry_name}</b><br>{hover_y_label}<extra></extra>"

        fig.add_trace(go.Scatter(
            x=[], y=[],
            mode='markers',
            marker=dict(color=custom_color, size=marker_size),
            name=entry_name,
            hovertemplate=hovertemplate,
            showlegend=False
        ))

    # --- Create Frames ---
    frames = []
    for gw in range(1, max_gw + 1):
        frame_data = []
        
        for eid in all_entries:
            # Filter data up to cumulative GW
            entry_history = df[(df['Entry ID'] == eid) & (df['GW'] <= gw)].sort_values('GW')
            
            # Line Data (Full history up to GW)
            x_line = entry_history['GW'].tolist()
            y_line = entry_history[y_col].tolist()
            
            # Tip Data (Current GW only)
            current = entry_history[entry_history['GW'] == gw]
            x_tip = current['GW'].tolist()
            y_tip = current[y_col].tolist()
            
            # Custom Data for Hover (Points, Delta)
            custom_data_tip = None
            if not current.empty:
                custom_data_tip = [
                    [current.iloc[0]['Total Points'], current.iloc[0]['Delta']]
                ]
            
            frame_data.append(go.Scatter(x=x_line, y=y_line)) # Update Line
            frame_data.append(go.Scatter(x=x_tip, y=y_tip, customdata=custom_data_tip))   # Update Tip
        
        frames.append(go.Frame(data=frame_data, name=str(gw)))

    fig.frames = frames

    # --- Layout with Sliders & Buttons ---
    steps = []
    for gw in range(1, max_gw + 1):
        step = dict(
            method="animate",
            args=[
                [str(gw)], # Frame name
                dict(
                    mode="immediate",
                    frame=dict(duration=300, redraw=True),
                    transition=dict(duration=0)
                )
            ],
            label=str(gw)
        )
        steps.append(step)

    sliders = [dict(
        active=0,
        currentvalue={"prefix": "Gameweek: "},
        pad={"t": 50},
        steps=steps
    )]
    
    fig.update_layout(
        title=title_text,
        xaxis_title="Gameweek",
        yaxis_title=metric,
        yaxis_autorange=y_autorange,
        xaxis=dict(range=[0.5, max_gw + 0.5]),
        yaxis=dict(range=y_range),
        hovermode="closest",
        height=600,
        template="plotly_dark",
        updatemenus=[dict(
            type="buttons",
            showactive=False,
            buttons=[dict(
                label="Play",
                method="animate",
                args=[None, dict(frame=dict(duration=500, redraw=True), fromcurrent=True)]
            ), dict(
                label="Pause",
                method="animate",
                args=[[None], dict(frame=dict(duration=0, redraw=False), mode="immediate", transition=dict(duration=0))]
            )]
        )],
        sliders=sliders,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        )
    )
    
    # Init with last frame to show completed season by default
    if frames:
        last_frame = frames[-1]
        for i, trace in enumerate(fig.data):
            trace.x = last_frame.data[i].x
            trace.y = last_frame.data[i].y
        
        # Set slider to last
        sliders[0]['active'] = len(steps) - 1

    return fig

def create_snapshot_chart(df_snap, impact_metric):
    """
    Creates a static Plotly scatter plot for Template vs Differentials.
    
    Args:
        df_snap: DataFrame with snapshot data.
        impact_metric: "Form", "Points Per Game", or "Cost"
        
    Returns:
        go.Figure: The plotly figure object.
    """
    metric_col = 'form'
    if impact_metric == "Points Per Game":
        metric_col = 'ppg'
    elif impact_metric == "Cost":
        metric_col = 'cost'
    
    # Colors
    def get_color(row):
        if row['is_captain']: return "#FFD700" # Gold
        if row['is_mine']: return "#1f77b4" # Blue
        return "#d3d3d3" # Grey
    
    # Use copy to avoid SettingWithCopy warnings if df_snap is a view
    df_snap = df_snap.copy()
    
    df_snap['color'] = df_snap.apply(get_color, axis=1)
    df_snap['opacity'] = df_snap.apply(lambda r: 1.0 if r['is_mine'] else 0.5, axis=1)
    df_snap['size'] = df_snap.apply(lambda r: 15 if r['is_captain'] else (10 if r['is_mine'] else 6), axis=1)
    
    fig_snap = go.Figure()
    
    # Add zones (Shapes)
    # Differential < 15%
    fig_snap.add_vrect(
        x0=0, x1=config.SNAPSHOT_DIFF_THRESHOLD, 
        fillcolor="green", opacity=0.1, 
        layer="below", line_width=0,
        annotation_text="Differentials", annotation_position="top left"
    )
    
    # Template > 60%
    fig_snap.add_vrect(
        x0=config.SNAPSHOT_TEMP_THRESHOLD, x1=100, 
        fillcolor="red", opacity=0.1, 
        layer="below", line_width=0,
        annotation_text="Template", annotation_position="top right"
    )
    
    # Scatter Trace
    fig_snap.add_trace(go.Scatter(
        x=df_snap['ownership_pct'],
        y=df_snap[metric_col],
        mode='markers',
        marker=dict(
            color=df_snap['color'],
            size=df_snap['size'],
            opacity=df_snap['opacity'],
            line=dict(width=1, color='DarkSlateGrey')
        ),
        text=df_snap['web_name'],
        hovertemplate=(
            "<b>%{text}</b><br>" +
            "Ownership: %{x:.1f}% (%{customdata[0]} owners)<br>" +
            f"{impact_metric}: %{{y}}<br>" +
            "<extra></extra>"
        ),
        customdata=df_snap[['owners_count']]
    ))
    
    # Calculate Y-axis range with buffer to clear top annotations
    max_y = df_snap[metric_col].max()
    y_upper = max_y * 1.2 if pd.notnull(max_y) and max_y > 0 else 10
    
    fig_snap.update_layout(
        title=f"Ownership vs {impact_metric}",
        xaxis_title="Ownership %",
        yaxis_title=impact_metric,
        template="plotly_dark",
        height=500,
        xaxis=dict(range=[-2, 102]), # Add padding
        yaxis=dict(
            range=[0, y_upper],
            dtick=config.CHART_Y_AXIS_DTICK,
            tick0=0
        ),
        showlegend=False
    )
    
    return fig_snap

def create_race_bar_chart(df, user_id, mode):
    """
    Create a horizontal bar chart for the League Race.
    
    Args:
        df: DataFrame with columns [entry_id, player_name, entry_name, points, rank]
        user_id: The ID of the current user (for highlighting)
        mode: 'total' or 'gw' (affects scaling/labels)
        
    Returns:
        go.Figure: Plotly figure
    """
    # Sort for chart (Top rank at top means simplified: sort ascending points means bottom drawn first? 
    # Plotly barh: y-axis category order.
    # We want Rank 1 at Top.
    # If we pass y=names, x=points.
    
    # Sort DF so that highest points is last (drawn at top if yaxis autorange=reversed? No default is bottom-up)
    # Actually, easiest is: Sort by points ascending. Then Rank 1 is last row.
    df_chart = df.sort_values('points', ascending=True).copy()
    
    # Identify user row
    user_row = df_chart[df_chart['entry_id'] == user_id]
    user_points = user_row['points'].iloc[0] if not user_row.empty else 0
    
    # Calculate colors
    colors = []
    text_labels = []
    
    # Logic for top 3 emphasis (if user is not in top 3)
    # We need to know who is top 3.
    top_3_ids = df.sort_values('points', ascending=False).head(3)['entry_id'].tolist()
    
    for _, row in df_chart.iterrows():
        eid = row['entry_id']
        pts = row['points']
        rank = row.get('gw_rank') if mode == 'gw' else row['rank']
        
        # Color
        if eid == user_id:
            colors.append('#FFD700') # Gold for user
        elif eid in top_3_ids:
            colors.append('#38003c') # FPL Dark Purple for leaders
        else:
            colors.append('#BEBEBE' if mode == 'total' else '#87CEEB') # Grey or Light Blue
            
        # Text Label (Inside Bar)
        gap = pts - user_points
        gap_str = f"{gap:+d}" if gap != 0 and user_row is not None else ""
        if eid == user_id: gap_str = "(You)"
        
        label = f"#{rank} {row['entry_name']} ({pts})"
        # If wide enough, append gap. We'll use hover for detailed gap mostly.
        text_labels.append(label)
        
    fig = go.Figure()
    
    fig.add_trace(go.Bar(
        x=df_chart['points'],
        y=df_chart['player_name'] + " (" + df_chart['entry_name'] + ")", # Unique Y keys
        orientation='h',
        marker_color=colors,
        text=text_labels,
        textposition='auto',
        hoverinfo='text',
        hovertext=[
            f"<b>{r['entry_name']}</b><br>Manager: {r['player_name']}<br>Points: {r['points']}<br>Gap: {r['points']-user_points:+d}" 
            for _, r in df_chart.iterrows()
        ],
        customdata=df_chart['entry_id'] # For click events
    ))
    
    # Add Gap Line (Vertical line at User Points)
    fig.add_shape(
        type="line",
        x0=user_points, y0=-0.5,
        x1=user_points, y1=len(df_chart) - 0.5,
        line=dict(color="red", width=2, dash="dash"),
    )
    
    fig.add_annotation(
        x=user_points,
        y=len(df_chart),
        text="Your Score",
        showarrow=False,
        yshift=10,
        font=dict(color="red", size=10)
    )
    
    title_text = "Season Points Ladder" if mode == 'total' else "Live Gameweek Ladder"
    
    fig.update_layout(
        title=title_text,
        xaxis_title="Points",
        template="plotly_dark",
        height=max(400, len(df_chart) * 25), # Auto-height
        margin=dict(l=10, r=10, t=50, b=50),
        yaxis=dict(
            showticklabels=False, # We put labels inside bars
        ),
        clickmode='event+select'
    )
    
    return fig
