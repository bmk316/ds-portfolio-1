
import json
from collections import defaultdict
import config
import requests
import pandas as pd
from pandas import DataFrame



"""

"""


API_KEY = config.api_key

class NYTimes:

    def __init__(self, *args):
        self.month, self.day, self.year, self.subsection_key = args
        self.apikey = API_KEY
        self.clean_data = self.SetupClient(self.year, self.month, self.apikey)
        self.clean_string = self.StructuredString(self.clean_data)




    def StructuredString(self, clean_data):
        struct_string = "These are the relevant topics: \n"
        for data in clean_data:
            headline, topic, website = data
            struct_string = struct_string + "Headline: {0:.50s}  \n Website can be found: {1} \n\n".format(headline, website)

        print(struct_string)

    def SetupClient(self, year, month, apikey):
        url = "http://api.nytimes.com/svc/archive/v1/"+str(year)+"/"+str(month)+".json?api-key="+apikey

        rdata = requests.get(url).text
        jdata = json.loads(rdata)

        all_articles = jdata['response']['docs']
        return self.SpecificTimeframe(all_articles, self.day)


    def SpecificTimeframe(self, all_data, day):
        list_of_dates = []

        for i, data in enumerate(all_data):

            #only want the dates that align with the debate nights
            published_date = data['pub_date']
            published_date = int(published_date[8:10])

            if (published_date == int(day)):
                main_title = all_data[i+1]['headline']['main']
                subsection = all_data[i+1]['subsection_name']
                url = all_data[i+1]['web_url']

                list_of_dates.append([main_title, subsection, url])

        #return list_of_dates
        return self.SpecificSubsection(list_of_dates)

    def SpecificSubsection(self, data_of_dates):

        #List of all the values that we have to focus on the given topic
        list_subsection = []

        #List of the given dates
        for i, article in enumerate(data_of_dates):
            title, subsection, url = article

            #Extracting the specific subtopics
            if subsection == self.subsection_key:
                list_subsection.append(article)

        return list_subsection

if __name__ == "__main__":
    NYTimes()


    #complete_articles = return_query()
