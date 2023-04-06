import csv 
'''
this script will overwrite the existing file so if something needs to be corrected or added we can just run it again
'''
study_metadata = [
  ['Study name', 'Wave', 'N', 'Source'],
  ['Growing Up in Ireland', '3', '91', 'https://doi.org/10.1177/1362361320981314'],
  ['Longitudinal Study of Australian Children (K)', '7', '58', 'https://doi.org/10.1177/0004867415595287'],
  ['Longitudinal Study of Australian Children (B)', '7', '107', 'https://doi.org/10.1177/0004867415595287'],
  ['Millenium Cohort Study', '5', '581', 'https://doi.org/10.1177/1362361320913671'],
  ['Howlin-Rutter', '2', '68' 'https://doi.org/10.1111/j.1469-7610.2004.00215.x']
]

with open('study_meta_data.csv', 'w', newline ='') as csvfile:
  writer = csv.writer(csvfile)
  writer.writerows(study_metadata)
