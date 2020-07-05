import pandas as pd
import datetime

# df = pd.read_csv('planilha_adis_raw.csv')
# df.iloc[2225:2234, 1] = df.iloc[2225:2234, 1].str.slice(13,)
# df.iloc[2225:2234, 2] = df.iloc[2225:2234, 2].str.slice(12,)
# df.loc[df["data_julgamento"] == "0/04/2020"] = "20/04/2020"
# df.to_csv('planilha_adis_raw_f.csv', index=False)
# df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
# df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)
# lista = df[df.lista == True]
# df.to_csv('planilha_adis_raw_datefixed.csv', index=False)
# df = pd.read_csv('planilha_adis_raw_date.csv')
# df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
# df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)
# d = {'True': True, 'False': False}
# df["lista"] = df["lista"].map(d)
# df["ocr"] = df["ocr"].map(d)
# df.iloc[1296, 14] = False
# df.to_csv('planilha_adis_raw_datefixedf.csv', index=False)
# value_to_check = pd.Timestamp(2020, 1, 1)
# before2020 = df['data_julgamento'] < value_to_check
# df2 = df[before2020]
# lista = df2[df2.lista == True].sort_values(by = "data_julgamento")
# df2.to_csv('planilha_até2020.csv',index=False)
# df = pd.read_csv('planilha_até2020.csv')
# lista = df[df.lista == True].sort_values(by = "data_julgamento")
# facil_de_raspar = df[df.link.str.contains("jurisprudencia")]
# d = {'Decisão': 0}
# du = df.fillna(value=d)
# unanime = du[du.Decisão.str.contains("unânime|unanimidade")]
# df = pd.read_csv('planilha2020_uv.csv')
# df.Ementa = df.Ementa.str.lower()
# df.Decisão = df.Decisão.str.lower()
# df.Legislação = df.Legislação.str.lower()
# df.Indexação = df.Indexação.str.lower()
# df.Observação = df.Observação.str.lower()
# df.acordao = df.acordao.str.lower()
# df["Acórdãos no mesmo sentido"] = df["Acórdãos no mesmo sentido"].str.lower()
# df.Doutrina = df.Doutrina.str.lower()
# df.rename(columns={'Ementa':'ementa', 'Decisão': "decisao", "Legislação":"legislacao", 'Indexação':'indexacao', 'Observação':'obs', "Acórdãos no mesmo sentido":"acordaos_mesmo_sentido", "Doutrina":"doutrina"}, inplace=True)
# df.to_csv('planilha_2020nuv.csv', index=False)
# df = pd.read_csv('planilha2020_tema_nuv.csv')
# penal = df[df.penal == True].sort_values(by = "data_julgamento")
# adm = df[df.administrativo == True].sort_values(by = "data_julgamento")
# trib = df[df.tributario == True].sort_values(by = "data_julgamento")
# const = df[df.constitucional == True].sort_values(by = "data_julgamento")
# df = pd.read_csv('planilha_adis_raw_date.csv')
# rel = pd.read_csv('relator.csv')
# r = pd.DataFrame(rel[["nome", "relator"]])
# df2 = pd.merge(df, r, how = "left")
# df2.relator = df2.relator.str.slice(17, )
# df2.to_csv('planilha_adis_raw_date+relator.csv', index=False)
# df = pd.read_csv('planilha_adis_raw_date+relator.csv')
# df.rename(columns={'Ementa':'ementa', 'Decisão': "decisao", "Legislação":"legislacao", 'Indexação':'indexacao', 'Observação':'obs', "Acórdãos no mesmo sentido":"acordaos_mesmo_sentido", "Doutrina":"doutrina"}, inplace=True)
# df.to_csv('planilha__adis_raw_r.csv', index=False)
# df2 = pd.read_csv('/Users/pldc/Python Projects/dissertação/planilhas_old/planilha_adis_raw.csv')
# df2.rename(columns={'Ementa':'ementa', 'Decisão': "decisao", "Legislação":"legislacao", 'Indexação':'indexacao', 'Observação':'obs', "Acórdãos no mesmo sentido":"acordaos_mesmo_sentido", "Doutrina":"doutrina"}, inplace=True)
# df.iloc[2233] = df2.iloc[2233]
# print(df.iloc[2233])
# df.to_csv('planilha2020_uv.csv', index=False)
# df = pd.read_csv('planilha2020_uv.csv')
# df.iloc[2233, 1] = df.iloc[2233, 1][12:]
# df.iloc[2233, 2] = df.iloc[2233, 2][12:]
# df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
# df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)
# df.to_csv('planilha2020_uv+.csv', index=False)
# df = pd.read_csv('planilha2020uv.csv')
# df.iloc[2225:2234, 8] = df.iloc[2225:2234, 8].str.slice(24, -1)
# sem_relator = df[df.relator.isna()]
# sr = sem_relator[["nome", "link"]]
# # sr.to_csv('sem_relator.csv', index=False)
# df.iloc[1042, 15] = "GILMAR MENDES"
# df.iloc[1876, 15] = "PAULO BROSSARD"
# df.iloc[2225, 15] = "ILMAR GALVÃO"
# df.iloc[2226, 15] = "MAURÍCIO CORRÊA"
# df.iloc[2227, 15] = "ILMAR GALVÃO"
# df.iloc[2228, 15] = "MARCO AURÉLIO"
# df.iloc[2229, 15] = "CARLOS VELLOSO"
# df.iloc[2230, 15] = "JOAQUIM BARBOSA"
# df.iloc[2231, 15] = "CELSO DE MELLO"
# df.iloc[2232, 15] = "ILMAR GALVÃO"
# df.iloc[2233, 15] = "CÁRMEN LÚCIA"
# df.iloc[2234, 15] = "LUIZ FUX"
# df.iloc[2235, 15] = "ROBERTO BARROSO"
# df.iloc[2236, 15] = "ALEXANDRE DE MORAES"
# df.iloc[2237, 15] = "CÁRMEN LÚCIA"
# df.iloc[2238, 15] = "EDSON FACHIN"
# df.iloc[2239, 15] = "ROBERTO BARROSO"
# df.iloc[2240, 15] = "CELSO DE MELLO"
# df.iloc[2241, 15] = "CELSO DE MELLO"
# df.iloc[2242, 15] = "DIAS TOFFOLI"
# df.iloc[2243, 15] = "CELSO DE MELLO"
# df.to_csv('planilha2020_uvx.csv', index=False)
# df = pd.read_csv('planilha_limpa.csv')
# df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
# df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)
# value_to_check = pd.Timestamp(2020, 1, 1)
# before2020 = df['data_julgamento'] < value_to_check
# df2 = df[before2020]
# df2.to_csv('planilha2020.csv', index=False)
# df = pd.read_csv('planilha_limpa.csv')
# df[df["relator"] == "CEZAR PELUSO (Presidente)"] = "CEZAR PELUSO"
# df[df["relator"] == "CEZAR PELUSO (PRESIDENTE)"] = "CEZAR PELUSO"
# df.to_csv('planilha_limpa_.csv', index=False)

## 2015-2019
# df = pd.read_csv('planilha2020.csv')
# df["data_julgamento"] = pd.to_datetime(df['data_julgamento'], dayfirst=True)
# df["data_publicacao"] = pd.to_datetime(df['data_publicacao'], dayfirst=True)
# v2019 = pd.Timestamp(2019, 1, 1)
# after2019 = df['data_julgamento'] > v2019
# df2019 = df[after2019]
# v2018 = pd.Timestamp(2018, 1, 1)
# after2018 = df['data_julgamento'] > v2018
# d2018 = df[after2018]
# before2019 = d2018['data_julgamento'] < v2019
# df2018 = d2018[before2019]
# v2017 = pd.Timestamp(2017, 1, 1)
# after2017 = df['data_julgamento'] > v2017
# d2017 = df[after2017]
# before2018 = d2017['data_julgamento'] < v2018
# df2017 = d2017[before2018]
# v2016 = pd.Timestamp(2016, 1, 1)
# after2016 = df['data_julgamento'] > v2016
# d2016 = df[after2016]
# before2017 = d2016['data_julgamento'] < v2017
# df2016 = d2016[before2017]
# v2015 = pd.Timestamp(2015, 1, 1)
# after2015 = df['data_julgamento'] > v2015
# d2015 = df[after2015]
# before2016 = d2015['data_julgamento'] < v2016
# df2015 = d2015[before2016]

df = pd.read_csv('planilha2020.csv')
df2 = df.sort_values(by = "data_julgamento")