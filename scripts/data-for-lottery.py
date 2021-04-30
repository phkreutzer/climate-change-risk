import pandas as pd

data = pd.read_csv('../data/data_climate-change-risk_2021-04-26_16-09.csv')
# removes uneccessary data
filtered_data = data[['CASE', 'DV01']]

# Recodes the collected data, in the original a donation of 0 was recorded as 1
filtered_data['donations'] = filtered_data.loc[:, 'DV01'] - 1 

filtered_data['CASE'].astype(str)
filtered_data['codes'] =[ ''.join(['D', str(CASE)]) for CASE in filtered_data['CASE']]

# removes non-responses and do-not participates
valid_data = filtered_data[filtered_data['donations'] >= 0]

valid_data['tickets'] = (10 - valid_data['donations']).astype(int)

valid_data['donations'] = valid_data['donations'].astype(int)
valid_data['donations'].describe()

lottery_data = valid_data.loc[:, ['codes', 'donations', 'tickets']].copy()

lottery_data.to_csv('../data/lottery_data.csv', index=False)