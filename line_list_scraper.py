from smartsheet import Smartsheet
from pprint import pprint

# spin up a Smartsheet client
smartsheet = Smartsheet('YOUR_API_KEY_HERE')

# working in this workspace
nursing_homes = smartsheet.Workspaces.get_workspace(6336279454476164)

# let's get all the sheets in there IF they start with a digit (current convention)
sheet_list = [(s.id, s.name) for s in nursing_homes.sheets if s.name[0].isdigit()]

# the columns we're going to pull
to_find = {
    "Patient First Name": None,
    "Patient Last Name": None,
    "Date of Birth": None,
    "MDSS ID": None,
    "Resident/Staff": None,
    "Order Date": None,
    "Test Location": None,
    "Result": None,
    "Status (Alive/Died)": None,
    "Test Location": None
}

def find_column_ids(columns):
    """We need to find the Column ID for the column titles we want. Returns a dict of title: ID"""
    
    # make a copy
    ids_to_return = to_find.copy()
    
    for c in columns:
        if c.title in to_find.keys():
            ids_to_return[c.title] = c.id
            
    return ids_to_return


def process_row(row, cols):
    """Process one row and return a dict of values"""
    
    row_to_return = to_find.copy()
    
    # reverse the lookup
    col_lookup = {v: k for k, v in cols.items()}
    
    for c in row.cells:
        if c.column_id in col_lookup.keys():
            row_to_return[col_lookup[c.column_id]] = c.value
        
    if row_to_return == to_find:
        return None
    else:
        return row_to_return

def get_testing_sheet(sheet_id):
    """Process one testing sheet and return all its processed rows"""
    
    # Get the sheet
    sheet = smartsheet.Sheets.get_sheet(sheet_id)
    
    # Look up the column IDs
    cols = find_column_ids(sheet.columns)
    
    # empty list for the processed rows
    processed_rows = []
    
    # iterate thru rows
    for r in sheet.rows:
        # process the row, using the column IDs from earlier
        p = process_row(r, cols)
        # if we get a result:
        if p:
            # attach two columns about the sheet
            p['Sheet ID'] = sheet_id
            p['Sheet Name'] = sheet.name
            # append it to the list
            processed_rows.append(p)
        else:
            pass

        
    # return the rows we processes
    return processed_rows

# now we want all rows from all sheets: empty list
all_rows = []

# iterate through all sheets
for s in sheet_list:
    
    # get the processed rows
    rows_in_sheet = get_testing_sheet(s[0])
    # tack it on to all_rows
    all_rows = all_rows + rows_in_sheet

# write the CSV
import csv

with open('/tmp/nursing_home_tests.csv', 'w') as f:
    dw = csv.DictWriter(f, fieldnames=all_rows[0].keys())
    dw.writeheader()
    dw.writerows(all_rows)