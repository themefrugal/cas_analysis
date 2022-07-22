import streamlit as st
import pandas as pd
import casparser
from src.params_local import *
from io import StringIO


# Base file to create a streamlit application using cas_parser
# Source: https://github.com/codereverser/casparser
# To do: Also need to compare with any MF using mfapi.in

@st.cache
def get_data(file_path):
    data_csv = casparser.read_cas_pdf(file_path, "", output='csv')
    df_data = pd.read_csv(StringIO(data_csv))
    return df_data

df_full = get_data(file_path)
df_data = df_full.copy()
df_data['folio_scheme'] = df_data['folio'].astype(str) + ' ' + df_data['scheme']

sel_folio = st.sidebar.selectbox("Select Folio:", df_data['folio_scheme'].unique())
df_folio = df_data[df_data['folio_scheme'] == sel_folio]

st.write(df_folio)
st.write('Net Profit:')
st.write(-df_folio.amount.sum().round(2))
