################################################
## Python examples
################################################


# Apps data list
apps_data = [['id',
  'track_name',
  'size_bytes',
  'currency',
  'price',
  'rating_count_tot',
  'rating_count_ver',
  'user_rating',
  'user_rating_ver',
  'ver',
  'cont_rating',
  'prime_genre',
  'sup_devices.num',
  'ipadSc_urls.num',
  'lang.num',
  'vpp_lic'],
 ['284882215',
  'Facebook热播',
  '389879808',
  'USD',
  '0.0',
  '2974676',
  '212',
  '3.5',
  '3.5',
  '95.0',
  '4+',
  'Social Networking',
  '37',
  '1',
  '29',
  '1'],
 ['389801252',
  'Instagram',
  '113954816',
  'USD',
  '0.0',
  '2161558',
  '1289',
  '4.5',
  '4.0',
  '10.23',
  '12+',
  'Photo & Video',
  '37',
  '0',
  '29',
  '1'],
 ['529479190',
  'Clash of Clans™™™',
  '116476928',
  'USD',
  '0.0',
  '2130805',
  '579',
  '4.5',
  '4.5',
  '9.24.12',
  '9+',
  'Games',
  '38',
  '5',
  '18',
  '1'],
 ['529479190',
  'Instachat 😜',
  '116476928',
  'USD',
  '0.0',
  '2130805',
  '579',
  '4.5',
  '4.5',
  '9.24.12',
  '9+',
  'Games',
  '38',
  '5',
  '18',
  '1'],
  ['284882215',
  '爱奇艺PPS -《欢乐颂2》电视剧热播',
  '389879808',
  'USD',
  '0.0',
  '2974676',
  '212',
  '3.5',
  '3.5',
  '95.0',
  '4+',
  'Social Networking',
  '37',
  '1',
  '29',
  '1'],
  ['389801252',
  'Docs To Go™ Free Office Suite',
  '113954816',
  'USD',
  '0.0',
  '2161558',
  '1289',
  '4.5',
  '4.0',
  '10.23',
  '12+',
  'Photo & Video',
  '37',
  '0',
  '29',
  '1']]

apps_data_NEW = ['id_NEW',
  'track_name_NEW',
  'size_bytes_NEW',
  'currency_NEW',
  'price_NEW',
  'rating_ctot',
  'rating_cver',
  'rating',
  'rating_ver',
  'ver',
  'cont_rating',
  'prime_genre',
  'sup_devices',
  'ipadSc_urls.',
  'lang',
  'vpp']

# Insert new header
apps_data.insert(0, apps_data_NEW)


# Print eleventh item in first list
print(apps_data[0][10])


# Print second item in third list
print(apps_data[2][1])


# Print all characters in ASCII integer format
test_text = 'Instachat😜'
for i in test_text:
    print(ord(i))


# Print max of characters string in ASCII integer format
ord(max(test_text))


# Loop over names from apps_data
# FOR DATAQUEST PROJECT
for j in apps_data[1:]:
  char_str = j[1]
  non_eng_char = 0
  for i in char_str:
    ASCII = ord(i)
    if ASCII > 127:
      non_eng_char += 1
  if non_eng_char > 2:
    print('> 2 non-English')
  else:
    print('<= 2 non-English')
    

# Print max of characters string in ASCII integer format
print(ord(max(apps_data[1][1])))


# GENERAL EXAMPLES
# For loop to list comprehension
# For loop
numbers = [1, 2, 3, 4, 5]
doubled_odds_fl = []
for n in numbers:
  if n % 2 == 1:
    doubled_odds_fl.append(n * 2)
print(doubled_odds_fl)

# List comprehension
doubled_odds_lc = [
  n * 2 
  for n in numbers 
  if n % 2 ==1
  ]
print(doubled_odds_lc)


# Nested for loop to list comprehension
# Nested for loop
flattened_nfc = []
data = [[1, 2, 3],['a', 'b', 'c']]
for row in data:
  for n in row:
    flattened_nfc.append(n)
print(flattened_nfc)


# List comprehension
flattened_lc = [
  n 
  for row in data 
  for n in row
  ]
print(flattened_lc)


# String replace loop
test_data1 = "c. 1912"
test_data2 = "(1988)"
test_data3 = "C. 1990-1999"
test_data4 = '1990-1999'

bad_chars = ["(",")","c","C",".","s","'", " "]

def strip_characters(string):
    for char in bad_chars:
        string = string.replace(char,"")
    return(string)

print(strip_characters(test_data3))

split_test = round(sum([int(i) for i in test_data4.split("-")]) / 2)
print(split_test)

# Convert dates
stripped_test_data = ['1912', '1929', '1913-1923',
                      '1951', '1994', '1934',
                      '1915', '1995', '1912',
                      '1988', '2002', '1957-1959',
                      '1955', '1970', '1990-1999']


def process_date1(list):
  return_list = []
  for i in list:
    if "-" in i:
        year = round(sum([int(i) for i in i.split("-")]) / 2)
    else:
        year = int(i)
    
    return_list.append(year)
  return return_list

print(process_date1(stripped_test_data))

def process_date(string):
    if "-" in string:
        year = round(sum([int(i) for i in string.split("-")]) / 2)
    else:
        year = int(string)
    return year

print(process_date(test_data4))

# Enumerate
for num, year in enumerate(stripped_test_data, start=1):
  print("Year {}: {}".format(num, year))


# Pandas iloc example
import numpy as np
import pandas as pd

df = pd.DataFrame(apps_data[1:], columns=apps_data[0])
df.iloc[0:4, np.r_[3, 1:2, 5:7]]