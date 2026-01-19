# -*- coding: utf-8 -*-
 
import pandas as pd
import argparse
 
def convert_csv_to_excel(input_file, output_file, separator):
    # Carica il CSV usando pandas con il separatore specificato
    # Usa engine='python' per tolleranza con campi malformati
    # on_bad_lines='skip' per saltare righe con numero di campi errato
    df = pd.read_csv(input_file, sep=separator, engine='python', on_bad_lines='skip')
   
    # Salva il DataFrame come file Excel
    df.to_excel(output_file, index=False)
    print(f"File Excel salvato come: {output_file}")
 
def main():
    # Crea un parser per la linea di comando
    parser = argparse.ArgumentParser(description="Converte un file CSV in un file Excel.")
   
    # Aggiungi gli argomenti necessari
    parser.add_argument("input_file", help="Il percorso del file CSV di input")
    parser.add_argument("output_file", help="Il percorso del file Excel di output")
    parser.add_argument("-s", "--separator", default=",", help="Il separatore del file CSV (default è ',')")
   
    # Parsea gli argomenti dalla linea di comando
    args = parser.parse_args()
   
    # Esegui la conversione
    convert_csv_to_excel(args.input_file, args.output_file, args.separator)
 
if __name__ == "__main__":
    main()
