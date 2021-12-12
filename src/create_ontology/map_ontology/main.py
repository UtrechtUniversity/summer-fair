import pandas as pd
import click
from mappings import Mappings
from ontology import Ontology
from pathlib import Path

@click.command()
@click.option('--config', required=True,type=Path,
			  help='path to mapping file')
@click.option('--filename', required=True,type=Path,
			  help='path to dataset')
@click.option('--outdir',type=Path, default='linked_data',
			  help='path to output folder')

def read_tabular(filename):
    """Read tabular data in Dataframe"""
    try:
        if filename.suffix == '.xlsx':
            dataset = pd.read_excel(filename,engine='openpyxl').fillna(method='ffill',axis=0).fillna('')
        elif filename.suffix == '.xls':
            dataset = pd.read_excel(filename).fillna(method='ffill',axis=0).fillna('')
        elif filename.suffix == '.csv':
            dataset = pd.read_csv(filename)
    except (UnicodeDecodeError,IndexError,ValueError) as e:
        print(f'Failed reading {filename} with error msg {e} ')
    except FileNotFoundError:    
        print(f'File {filename} does not exist')

    return dataset

def check_outdir(outdir):
    """Create outdir if it does not exist"""
    if outdir.exists():
        if outdir.is_file():
            raise RuntimeError('Output path must be a folder')
    else:
        outdir.mkdir(parents=True, exist_ok=True)


def main(config,filename,outdir):
    ont_file = 'trans_ont.owl'

    dataset = read_tabular(filename)
    columns = dataset.columns.values.tolist()

    # create mapping and ontology class
    mappings = Mappings(config, columns)
    ontology = Ontology(ont_file)

    for _, row in dataset.iterrows():
        # check for the required field
        # if it doesn't exit then run it for each row
        if mappings.required_field is None or row[mappings.required_field]:
            ontology.populate_ontology(mappings, row,columns)

    # save populated ontology
    ontology.save_ontology(f'{outdir/filename.stem}.ttl')

if __name__ == '__main__':
    main()
