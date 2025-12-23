import streamlit as st
import pandas as pd
import plotly.graph_objects as go
import league_replay
import config
import visualizations

def render_my_team_comparison(selected_league_id, entry_id, member_map):
    st.header("My Team Comparison")
    st.markdown("Compare your team against the template of your mini-league.")

    # Manager Selection (Defaults to User)
    manager_options = [(eid, name) for eid, name in member_map.items()]
    
    # Default index
    default_idx = 0
    if entry_id:
        for i, (eid, _) in enumerate(manager_options):
            if eid == entry_id:
                default_idx = i
                break
                
    col1, col2 = st.columns(2)
    with col1:
        selected_manager_tuple = st.selectbox(
            "Select Focus Manager",
            options=manager_options,
            format_func=lambda x: x[1],
            index=default_idx
        )
        selected_manager_id = selected_manager_tuple[0]
        
    with col2:
        impact_metric = st.selectbox(
            "Impact Metric (Y-Axis)",
            ["Form", "Points Per Game", "Cost"],
            index=1 # Default to PPG
        )

    if st.button("Generate Snapshot", type="primary"):
        with st.spinner(f"Fetching live picks for {len(member_map)} managers..."):
            snap_data, err = league_replay.get_league_snapshot_data(selected_league_id, selected_manager_id)
            
        if err:
            st.error(err)
        else:
            df_snap = snap_data['df']
            current_gw = snap_data['gw']
            num_mgrs = snap_data['num_managers']
            
            st.subheader(f"Gameweek {current_gw} Analysis ({num_mgrs} Managers)")
            
            # --- Visualization ---
            fig_snap = visualizations.create_snapshot_chart(df_snap, impact_metric)
            st.plotly_chart(fig_snap, use_container_width=True)
            
            # --- Key Takeaways ---
            st.markdown("### Key Takeaways")
            col_k1, col_k2, col_k3 = st.columns(3)
            
            # 1. Most Owned
            top_owned = df_snap.sort_values('ownership_pct', ascending=False).head(5)
            with col_k1:
                st.write("**Top 5 Owned**")
                for _, r in top_owned.iterrows():
                    st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
            
            # 2. My Differentials
            my_diffs = df_snap[(df_snap['is_mine']) & (df_snap['ownership_pct'] < config.SNAPSHOT_DIFF_THRESHOLD)]
            with col_k2:
                st.write("**Your Differentials (<15%)**")
                if not my_diffs.empty:
                    for _, r in my_diffs.iterrows():
                        st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
                else:
                    st.write("No low-ownership differentials.")
                    
            # 3. Threats
            threats = df_snap[(~df_snap['is_mine']) & (df_snap['ownership_pct'] > 40)].sort_values('ownership_pct', ascending=False).head(5)
            with col_k3:
                st.write("**Main Threats (Not Owned, >40%)**")
                if not threats.empty:
                    for _, r in threats.iterrows():
                        st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
                else:
                    st.write("You own the main template players.")


def render_my_league_position(selected_league_id, entry_id, df_league):
    st.header("My League Position")
    st.markdown("Visualize the race for the title.")

    max_gw = df_league['GW'].max()
    all_entries = df_league['Entry ID'].unique()
    
    # --- Multi-Manager Highlighting ---
    name_to_id = {}
    id_to_name = {}
    for eid in all_entries:
        row = df_league[df_league['Entry ID'] == eid].iloc[0]
        label = f"{row['Name']} [{eid}]"
        name_to_id[label] = eid
        id_to_name[eid] = label
        
    # Default selection: The current user
    default_label = id_to_name.get(entry_id)
    default_selection = [default_label] if default_label else []
    
    st.subheader("Highlight Managers")
    selected_labels = st.multiselect(
        "Compare against:", 
        options=name_to_id.keys(),
        default=default_selection
    )
    
    selected_ids = [name_to_id[l] for l in selected_labels]
    
    # Color Palette
    PALETTE = [
        "#00FFFF", "#FF00FF", "#FFFF00", "#00FF00", 
        "#FF4500", "#1E90FF", "#FF1493", "#ADFF2F"
    ]
    
    highlight_config = {}
    for idx, eid in enumerate(selected_ids):
        color = PALETTE[idx % len(PALETTE)]
        highlight_config[eid] = color
        
    # --- Animation Metric Selector ---
    metric = st.radio("Metric", ["Rank", "Gap to Leader"], horizontal=True)

    # --- Visualization ---
    fig = visualizations.create_league_animation_chart(df_league, highlight_config, metric)
    st.plotly_chart(fig, use_container_width=True)

    # --- GIF Export ---
    with st.expander("Export Animation"):
        if not league_replay.KALEIDO_AVAILABLE:
            st.warning("GIF export is not available. The `kaleido` library is required but not installed.")
        elif st.button("Generate Animation (GIF)"):
            with st.spinner("Generating GIF..."):
                progress_bar = st.progress(0)
                gif_bytes = league_replay.generate_league_gif(
                    df_league, 
                    highlight_config, 
                    progress_callback=lambda p: progress_bar.progress(p)
                )
                progress_bar.empty()
                st.download_button(
                    label="Download GIF",
                    data=gif_bytes,
                    file_name="league_replay.gif",
                    mime="image/gif"
                )


def main():
    st.set_page_config(layout="wide", page_title="My FPL Mini Leagues")
    
    st.title("My FPL Mini Leagues")
    st.markdown("Deep dive into your mini-leagues. Compare your team and track the title race.")
    
    # --- Step 1: User Input ---
    with st.container():
        col1, col2 = st.columns([1, 2])
        with col1:
            entry_id_input = st.text_input("Enter your Team ID", help="Found in your FPL URL")
            
    if not entry_id_input:
        st.info("Please enter your Team ID to begin.")
        return

    if not entry_id_input.isdigit():
        st.error("Team ID must be a number.")
        return
        
    entry_id = int(entry_id_input)
    
    # --- Step 2: League Selection ---
    with st.spinner("Fetching team details..."):
        meta = league_replay.fetch_team_metadata(entry_id)
        
    if not meta:
        st.error("Could not fetch team details. Check the ID.")
        return
        
    leagues = league_replay.get_classic_leagues(meta)
    
    if not leagues:
        st.warning("No classic leagues found for this team.")
        return
        
    # League Selector
    league_options = {l['name']: l['id'] for l in leagues}
    selected_league_name = st.selectbox("Select a League", list(league_options.keys()))
    selected_league_id = league_options[selected_league_name]
    
    # Load League Data
    if 'current_league_id' not in st.session_state or st.session_state['current_league_id'] != selected_league_id:
        if st.button("Load League Data"):
             with st.spinner("Analyzing league history..."):
                df_league, error = league_replay.process_league_history(selected_league_id)
                if error:
                    st.error(error)
                    st.session_state['league_df'] = None
                else:
                    st.session_state['league_df'] = df_league
                    st.session_state['current_league_id'] = selected_league_id
                    st.rerun() # Rerun to show tabs immediately
    
    # Check if data is loaded
    if st.session_state.get('current_league_id') == selected_league_id and st.session_state.get('league_df') is not None:
        df_league = st.session_state['league_df']
        
        # Determine Member Map from the dataframe for the comparison tool
        # (Alternatively we could refetch, but the DF has names and IDs)
        # We need the full member list for the dropdown
        # The DF assumes we fetched everyone.
        
        all_entries = df_league[['Entry ID', 'Name']].drop_duplicates()
        member_map = dict(zip(all_entries['Entry ID'], all_entries['Name']))

        # --- Step 3: Tabs ---
        st.divider()
        tab1, tab2 = st.tabs(["My Comparison", "My League Position"])
        
        with tab1:
            render_my_team_comparison(selected_league_id, entry_id, member_map)
            
        with tab2:
            render_my_league_position(selected_league_id, entry_id, df_league)

if __name__ == "__main__":
    main()
