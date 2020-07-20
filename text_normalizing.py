import pandas as pd
import cleantext

df = pd.read_csv('/Users/pldc/R Projects/stf_text_mining/planilha2020_processada.csv')
df.ementa = df.ementa.apply(cleantext.clean, fix_unicode=True, to_ascii=False, no_line_breaks=True, lower=False)
df.ementa = df.ementa.str.replace(pat=r'(?i)^ementa[: ]', repl="", case=False, n=1)
df.acordao = df.acordao.apply(cleantext.clean, fix_unicode=True, to_ascii=False, no_line_breaks=True, lower=False, no_urls=True, replace_with_url="")
df.acordao = df.acordao.str.replace(pat=r'(?i)^.*?(?=ementa)', repl="", case=False, n=1)
df.decisao = df.decisao.apply(cleantext.clean, fix_unicode=True, to_ascii=False, no_line_breaks=True, lower=False)
df.to_excel('planilha2020_processada.xlsx', index = False)
df.to_csv('planilha2020_processada.csv', index=False)