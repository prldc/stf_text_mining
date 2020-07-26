import pandas as pd
import datetime

# FIXING MINOR IMPERFECTIONS
df = pd.read_csv('planilha_adis_raw.csv')
df.iloc[2225:2234, 1] = df.iloc[2225:2234, 1].str.slice(13,)
df.iloc[2225:2234, 2] = df.iloc[2225:2234, 2].str.slice(12,)
df.loc[df["data_julgamento"] == "0/04/2020"] = "20/04/2020"

# CONVERTING TO DATETIME
df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)

# STANDARDIZING BOOLEAN COLUMNS

df.iloc[1296, 14] = False

df.rename(columns={'Ementa':'ementa', 'Decisão': "decisao", "Legislação":"legislacao", 'Indexação':'indexacao', 'Observação':'obs', "Acórdãos no mesmo sentido":"acordaos_mesmo_sentido", "Doutrina":"doutrina"}, inplace=True)
df.to_csv('planilha_processada.csv', index=False)

# NOW RUN text_cleaning.R

