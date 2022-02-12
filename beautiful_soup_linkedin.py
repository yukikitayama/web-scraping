import requests
from bs4 import BeautifulSoup

GEOID = ''


# url = f'https://www.linkedin.com/jobs/search/?geoId={GEOID}&keywords=data%20scientist&location=California%2C%20United%20States'
url = f'https://www.linkedin.com/jobs/search/?currentJobId=2911331220&geoId={GEOID}&keywords=data%20scientist&location=California%2C%20United%20States'

r = requests.get(url)
print(r.text)
soup = BeautifulSoup(r.text, 'html.parser')
results = soup.find(id='job-details')
print(results)