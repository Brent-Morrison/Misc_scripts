import camelot
import pandas as pd

PDF_PATH = "https://www.vic.gov.au/sites/default/files/2019-08/Full-Report-Population-Diversity-in-LGAs-2016-Census-Web-version-30May18.PDF"
pages = list(range(109, 273)) + list(range(275, 351))
pdf_path = PDF_PATH

"""
This code uses the Camelot library to extract tables from a PDF document. It attempts to extract tables using both the "lattice"
and "stream" flavors of Camelot, which are different methods for detecting tables in PDFs.  The extracted tables are stored in a 
list called `all_tables`.  The code also handles exceptions that may occur during the extraction process and prints error messages 
if either method fails.
"""

#def extract_with_camelot(pdf_path, pages):

all_tables = []

page_str = ",".join(str(p + 1) for p in pages)  # Camelot uses 1-based

print("Running Camelot (lattice)...")
try:
    tables = camelot.read_pdf(
        pdf_path,
        pages=page_str,
        flavor="lattice",
        strip_text="\n"
    )
    for t in tables:
        df = t.df
        if not df.empty:
            all_tables.append(df)
except Exception as e:
    print("Lattice failed:", e)

print("Running Camelot (stream)...")
try:
    tables = camelot.read_pdf(
        pdf_path,
        pages=page_str,
        flavor="stream",
        strip_text="\n"
    )
    for t in tables:
        df = t.df
        if not df.empty:
            all_tables.append(df)
except Exception as e:
    print("Stream failed:", e)

 #   return all_tables




"""
This code processes the extracted tables to find those that match a specific type (in this case, "Table 2"). 
It concatenates the text from the first two rows of each table to identify the table name and extract the 
Local Government Area (LGA) name. If a table matches the specified type, it reshapes the data, sets appropriate 
column names, and converts numeric columns to the correct format. The processed tables are then concatenated 
into a single DataFrame called `table_2`.
 """

table = "Table 2"
dfs = []

for t in all_tables:

  # Concatenate all columns in the first and second row to find the Table name and LGA name, and also the last row to check if it contains "Total Persons"
  tr1 = ' '.join(t.iloc[0].fillna('').astype(str)).replace('\xa0', ' ').strip()
  tr2 = ' '.join(t.iloc[1].fillna('').astype(str)).replace('\xa0', ' ').strip()
  tr9 = ' '.join(t.iloc[-1].fillna('').astype(str)).replace('\xa0', ' ').strip()

  # If the table is of the type specified by 'table', extract the LGA name and reshape
  if table in tr1 and " (" in tr1:
    lga = tr1.split(table + " ", 1)[1].split(" (", 1)[0].strip()
  elif table in tr2 and " (" in tr2:
    lga = tr2.split(table + " ", 1)[1].split(" (", 1)[0].strip()
  else:
    lga = None

  if lga is not None:
    df = t.copy()
    df = df.iloc[3:]
    df.columns = df.iloc[0]
    df = df.iloc[1:].reset_index(drop=True)
    df.columns = ["country", "males_2016", "females_2016", "persons_2016", "xxx", "persons_2011", "xxx", "xxx"]
    df = df.drop(columns=["xxx"])
    
    # Drop last row if it does not contain the text "Total Persons"
    if "Total Persons" not in tr9:
        df = df.iloc[:-1] 

    # Convert columns to numeric
    cols = ["males_2016", "females_2016", "persons_2016", "persons_2011"]
    for c in cols:
      df[c] = pd.to_numeric(df[c].str.replace(',', ''), errors="coerce")
    
    # Identifer column
    df.insert(0, "lga", lga)
      
    # Append before concatenating
    dfs.append(df)

table_2 = pd.concat(dfs, ignore_index=True)

# Save to csv
table_2.to_csv("vic_popn_diversity_table_2_raw.csv", index=False)