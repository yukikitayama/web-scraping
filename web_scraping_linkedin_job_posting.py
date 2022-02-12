from selenium import webdriver
from selenium.webdriver.chrome.service import Service
import pandas as pd


EXECUTABLE_PATH = './chromedriver.exe'
GEOID = ''

url = f'https://www.linkedin.com/jobs/search/?geoId={GEOID}&keywords=data%20scientist&location=California%2C%20United%20States'
s = Service(EXECUTABLE_PATH)
wd = webdriver.Chrome(service=s)
wd.get(url)
job_lists = wd.find_element_by_class_name('jobs-search__results-list')
jobs = job_lists.find_element_by_tag_name('li')
print(len(jobs))

# wd = webdriver.Chrome(executable_path=EXECUTABLE_PATH)
# op = webdriver.ChromeOptions()
# wd = webdriver.Chrome(service=s, options=op)
