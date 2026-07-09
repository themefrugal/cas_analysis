from __future__ import annotations

import io
from datetime import date

import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from cas_streamlit.core.analytics import (
    analytics_group_label_to_col,
    available_custom_analytics_groups,
    ensure_gains_column,
    get_allocation_summary,
    get_performance_contributors,
    sanitize_custom_analytics_groups,
)
from cas_streamlit.core.market_data import extract_fund_name
from cas_streamlit.core.parser import CasParseError, sample_pdf_paths
from cas_streamlit.services.analysis import (
    compute_period_summary,
    filtered_transactions,
    fund_summary,
    hierarchy_table,
    run_analysis,
)


def _money_style(df: pd.DataFrame, columns: list[str] | None = None):
    columns = columns or [col for col in df.columns if df[col].dtype.kind in "fc"]
    return df.style.format({col: "{:,.2f}" for col in columns}, na_rep="N/A")


def _result():
    return st.session_state.get("analysis_result")


def main() -> None:
    st.set_page_config(page_title="Mutual Fund Analysis and Report", layout="wide")
    st.title("Mutual Fund Analysis and Report")

    with st.sidebar:
        st.header("CAS Analysis")
        uploaded_file = st.file_uploader("CAS PDF", type="pdf")
        password = st.text_input("PDF password", type="password")
        sample_paths = sample_pdf_paths()
        sample_choice = st.selectbox(
            "Sample PDFs",
            [""] + sample_paths,
            format_func=lambda path: "Choose a sample" if not path else path.split("/")[-1],
        )
        analyze = st.button("Analyze", type="primary", width="stretch")

        if analyze:
            source = uploaded_file
            if sample_choice:
                source = sample_choice
                password = ""
            if source is None:
                st.warning("Upload a CAS PDF or choose a sample PDF.")
            else:
                try:
                    progress = st.progress(0, text="Starting analysis...")
                    counter = {"i": 0}

                    def progress_fn(fund: str) -> None:
                        counter["i"] += 1
                        progress.progress(0.25, text=f"Fetching NAV data: {extract_fund_name(fund)[:60]}")

                    result = run_analysis(source, password, progress_fn=progress_fn)
                    st.session_state.analysis_result = result
                    non_cv = result.dt_base_txns[result.dt_base_txns["description"] != "Cur Value"]["date"]
                    close_date = result.dt_base_txns[result.dt_base_txns["description"] == "Cur Value"]["date"].max()
                    st.session_state.start_date = non_cv.min().date()
                    st.session_state.end_date = close_date.date()
                    progress.empty()
                    st.success("PDF processed successfully.")
                except CasParseError as exc:
                    st.session_state.analysis_result = None
                    st.error(str(exc))
                except Exception as exc:
                    st.session_state.analysis_result = None
                    st.error(f"Analysis failed: {exc}")

        result = _result()
        if result is None:
            st.info("Waiting for a CAS PDF.")
        else:
            st.success("Analysis ready")
            start_default = st.session_state.get("start_date", date.today())
            end_default = st.session_state.get("end_date", date.today())
            st.session_state.start_date = st.date_input("Analysis period start", value=start_default)
            st.session_state.end_date = st.date_input("Analysis period end", value=end_default)
            if st.session_state.start_date > st.session_state.end_date:
                st.warning("Start date must be on or before end date.")

    result = _result()
    if result is None:
        st.info("Upload and analyze a CAS PDF to begin. Sample PDFs can be selected from the sidebar.")
        return

    start = st.session_state.start_date
    end = st.session_state.end_date
    tabs = st.tabs(
        [
            "Benchmark",
            "Portfolio",
            "Analytics",
            "Fund Detail",
            "Insights",
            "Transactions",
            "Report",
            "Help",
        ]
    )

    with tabs[0]:
        st.subheader("Benchmark")
        summary, period_xirr, warnings = compute_period_summary(result, start, end)
        xirr_label = "N/A" if period_xirr is None else f"{period_xirr * 100:.3f}%"
        st.metric("Analysis Period XIRR", xirr_label)
        st.dataframe(_money_style(summary, ["Amount"]), width="stretch", hide_index=True)
        for warning in warnings:
            st.warning(warning)
        st.caption("Benchmark simulation uses NAV on or before each cash-flow date to avoid exact-date gaps.")

    with tabs[1]:
        st.subheader("Portfolio")
        curve = result.portfolio_curve
        if curve.empty:
            st.info("Portfolio curve is unavailable.")
        else:
            fig = go.Figure()
            fig.add_trace(go.Scatter(x=curve["date"], y=curve["net_invested"], name="Amount Invested", fill="tozeroy"))
            fig.add_trace(go.Scatter(x=curve["date"], y=curve["portfolio_value"], name="Portfolio Value", fill="tonexty"))
            fig.update_layout(height=460, yaxis_title="Value", hovermode="x unified")
            st.plotly_chart(fig, width="stretch")
        xirr_curve = result.xirr_curve.dropna(subset=["xirr"])
        if not xirr_curve.empty:
            fig = go.Figure()
            fig.add_trace(go.Scatter(x=xirr_curve["date"], y=xirr_curve["xirr"], name="XIRR", mode="lines"))
            fig.update_layout(height=360, yaxis_title="XIRR (%)", hovermode="x unified")
            st.plotly_chart(fig, width="stretch")

    with tabs[2]:
        st.subheader("Analytics")
        labels = st.multiselect(
            "Group by",
            options=available_custom_analytics_groups(st.session_state.get("analytics_groups", ["AMC", "Category", "Scheme"])),
            default=sanitize_custom_analytics_groups(st.session_state.get("analytics_groups", ["AMC", "Category", "Scheme"])),
        )
        labels = sanitize_custom_analytics_groups(labels)
        st.session_state.analytics_groups = labels
        groups = analytics_group_label_to_col(labels)
        if groups:
            st.dataframe(hierarchy_table(result, groups), width="stretch", hide_index=True)
        else:
            st.info("Choose at least one grouping level.")

    with tabs[3]:
        st.subheader("Fund Detail")
        funds = sorted(result.dt_base_txns["fund"].dropna().unique())
        selected = st.selectbox("Fund", funds, format_func=extract_fund_name)
        st.write("Funds Summary")
        st.dataframe(_money_style(fund_summary(result, start, end)), width="stretch", hide_index=True)
        st.write("Drilldown")
        fund_rows = result.dt_base_txns[result.dt_base_txns["fund"] == selected]
        st.dataframe(fund_rows, width="stretch", hide_index=True)

    with tabs[4]:
        st.subheader("Insights")
        leaves = ensure_gains_column(fund_summary(result, start, end).rename(
            columns={"Fund": "Scheme", "Cur.Value": "Cur Value", "XIRR%": "XIRR%"}
        ))
        if leaves.empty:
            st.info("No insight data is available.")
        else:
            value_mode = st.radio("Display", ["Value", "Percentage"], horizontal=True)
            alloc = get_allocation_summary(leaves, "Scheme")
            y_col = "Weight" if value_mode == "Percentage" else "Cur Value"
            alloc = alloc.sort_values(y_col, ascending=False)
            fig = go.Figure([go.Bar(x=alloc["Group"], y=alloc[y_col])])
            fig.update_layout(height=420, yaxis_title="%" if value_mode == "Percentage" else "Value")
            st.plotly_chart(fig, width="stretch")
            contributors = get_performance_contributors(leaves)
            c1, c2 = st.columns(2)
            c1.dataframe(contributors["top"], width="stretch", hide_index=True)
            c2.dataframe(contributors["bottom"], width="stretch", hide_index=True)

    with tabs[5]:
        st.subheader("Transactions")
        txns = filtered_transactions(result, start, end)
        st.dataframe(txns, width="stretch", hide_index=True)
        csv = txns.to_csv(index=False).encode("utf-8")
        st.download_button("Download CSV", csv, "transactions.csv", "text/csv")
        buf = io.BytesIO()
        txns.to_excel(buf, index=False)
        st.download_button(
            "Download Excel",
            buf.getvalue(),
            "transactions.xlsx",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        )

    with tabs[6]:
        st.subheader("Report")
        show_health = st.toggle("Show health-check tables", value=False)
        st.dataframe(result.fund_match_log, width="stretch", hide_index=True)
        if show_health:
            st.write("Diagnostics")
            st.dataframe(result.diagnostics, width="stretch", hide_index=True)
            st.write("NAV Status")
            st.dataframe(result.nav_status_log, width="stretch", hide_index=True)

    with tabs[7]:
        st.subheader("Help")
        st.markdown(
            """
Use CAMS or KFintech to generate an encrypted Consolidated Account Statement.
Upload the PDF, enter its password, and analyze. The sample PDFs in the sidebar
use a blank password and contain fictional investor data.
"""
        )
