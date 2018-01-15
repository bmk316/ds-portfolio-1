import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sb
import numpy as np

'''
The headers are: 'sepal_len_cm', 'sepal_wid_cm', 'petal_len_cm', 'petal_wid_cm', 'class'
Classes are: Iris-setosa, Iris-versicolor, and Iris-virginica
'''
features = ['sepal_len_cm', 'sepal_wid_cm', 'petal_len_cm', 'petal_wid_cm']

def load_data():
    '''
    No headers were provided, had to create
    '''
    irisdata_df = pd.read_csv("iris.csv",
            names=['sepal_len_cm', 'sepal_wid_cm', 'petal_len_cm', 'petal_wid_cm', 'class'], na_values=['NA'])
    return irisdata_df


def unique_class(irisdata_df):
    return irisdata_df['class'].unique()


def using_specific_cls(irisdata_df, data_class):
    '''
    The first parameter after the == sign is the search term you are looking for followed
    by the column name you desire (if left empty, the entire dataframe is returned)
    '''
    return irisdata_df.loc[irisdata_df['class'] == data_class, 'class']


def dropping_empty_vals(irisdata_df):
    return irisdata_df.dropna(inplace=True)


def look_for_null_values(irisdata_df, features):
    potential_null_val = []

    for feature in features:
        null_values = irisdata_df.loc[irisdata_df[feature].isnull()]
        potential_null_val.append(null_values)

    return (potential_null_val)


def describe_data(irisdata_df):
    return irisdata_df.describe()


def look_for_outliers(irisdata_df, features):
    potential_outliers = []
    treshold = 3 # It's generally assumed that in a Gaussian distribution, anything over three std. is an outliers

    for feature in features:
        mean = np.mean(irisdata_df[feature])
        std = np.std(irisdata_df[feature])

        z_score = [(y - mean) / std for y in irisdata_df[feature]]
        absolute_z_scpre = np.abs(z_score)

        index_of_outliers = np.where(absolute_z_scpre > treshold)
        for index in index_of_outliers:
            potential_outliers.append(irisdata_df.iloc[index])

        return potential_outliers


def scatterplot_matrix(irisdata_df):
    """
    If values are empty, they are dropped
    """
    sb.pairplot(irisdata_df.dropna(), hue='class')
    plt.show()


def histograms(irisdata_df, data_class):
    hist = irisdata_df.loc[irisdata_df['class'] == data_class].hist()
    plt.show()


def violin_plot(irisdata_df):
    """
    Draws the plot for all columns except class...
    """
    plt.figure(figsize=(10, 10))

    for colm_index, colm in enumerate(irisdata_df.columns):
        if colm == 'class':
            continue
        plt.subplot(2, 2, colm_index+1)
        sb.violinplot(x='class', y=colm, data=irisdata_df)
    plt.show()




def understand_data(irisdata_df):
    '''
    outliers returns index of potential outliers
    '''
    #describe_data(irisdata_df)
    #look_for_outliers(irisdata_df, features)
    #scatterplot_matrix(irisdata_df)
    #histograms(irisdata_df, 'Iris-setosa')
    violin_plot(irisdata_df)


def cleaning_up_data(irisdata_df):
    """
    Check there are no type errors in labeling the labels
    Check a specific column
    Check to see if there are any null values
    """
    unique_class(irisdata_df)
    using_specific_cls(irisdata_df, 'Iris-setosa')
    dropping_empty_vals(irisdata_df)
    null_vals = look_for_null_values(irisdata_df, features) # Check this for any null values




def main():
    irisdata_df = load_data()
    #cleaning_up_data(irisdata_df)
    understand_data(irisdata_df)






if __name__ == "__main__":
    main()
