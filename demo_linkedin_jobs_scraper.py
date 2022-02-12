from linkedin_jobs_scraper import LinkedinScraper


EXECUTABLE_PATH = './chromedriver.exe'


scraper = LinkedinScraper(
    chrome_executable_path=EXECUTABLE_PATH,  # Custom Chrome executable path (e.g. /foo/bar/bin/chromedriver)
    chrome_options=None,  # Custom Chrome options here
    headless=True,  # Overrides headless mode only if chrome_options is None
    max_workers=1,  # How many threads will be spawned to run queries concurrently (one Chrome driver for each thread)
    slow_mo=1.3,  # Slow down the scraper to avoid 'Too many requests (429)' errors
)
