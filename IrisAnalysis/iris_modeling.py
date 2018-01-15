import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sb
import numpy as np

from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score

def perform_cross_validation(X_feat, Y_labels):
    """
    Our model will be done with DecisionTreeClassifier
    This model assumes that we have
    """
    decison_trees_classifier = DecisionTreeClassifier()

    cv_vals = cross_val_score(decison_trees_classifier, X_feat, Y_labels, cv=10)
    sb.distplot(cv_vals)
    plt.show()

def perform_cross_validation(X_feat, Y_labels):
    """
    Our model will be done with DecisionTreeClassifier
    This model assumes that we have
    """
    decison_trees_classifier = DecisionTreeClassifier()

    cv_vals = cross_val_score(decison_trees_classifier, X_feat, Y_labels, cv=10)
    sb.distplot(cv_vals)
    plt.show()

def plot_cross_validation(all_labels, n_samples):
    '''
    Complicated:
    The premise of this is to get an undestanding of how the data is divided, with k-fold.
    As we see, we split the data into 10 parts (15 are used as test, and the remaining as train)

    A quick visual on how cross-validation works.
    '''
    masks = []
    kf = KFold(n_splits=10)
    for train_index, test_index in kf.split(all_labels):
        mask = np.zeros(n_samples, dtype=bool)
        mask[test_index] = 1
        masks.append(mask)

    plt.figure(figsize=(8, 8))
    plt.imshow(masks, interpolation='none')
    plt.ylabel('Fold')
    plt.xlabel('Row #')
    plt.show()



def repeat_model(input_feat, input_cls):
    """
    Because we need to know if our predicition score holds up, we repeat this
    numerous times and then plot to verify our accuracy predicition is resonably high

    Note: Don't mix up the order of the return values from the train_test_split

    """
    model_pred = []

    for repetition in range(1000):
        (train_input, test_input, train_class, test_class) = train_test_split(input_feat, input_cls, train_size=0.75)

        decison_trees_classifier = DecisionTreeClassifier()
        decison_trees_classifier.fit(train_input, train_class)
        pred_score = decison_trees_classifier.score(test_input, test_class)

        model_pred.append(pred_score)


def decison_trees(*args):
    '''
    Create a decision tree classifier, and then work with the train data
    to figure out the decision tree that should be applied on the test set.
    We then apply this on the test set.

    The pred_score is 0.973684210526. (High value of prediction)
    '''
    train_input, test_input, train_class, test_class = args

    decison_trees_classifier = DecisionTreeClassifier()
    decison_trees_classifier.fit(train_input, train_class)
    pred_score = decison_trees_classifier.score(test_input, test_class)


def split_dataset(input_feat, input_cls):
    (train_input, test_input, train_class, test_class) = train_test_split(input_feat, input_cls, train_size=0.75, random_state=1)
    return (train_input, test_input, train_class, test_class)


def store_features(irisdata_df):
    input_features = irisdata_df[['sepal_len_cm', 'sepal_wid_cm', 'petal_len_cm', 'petal_wid_cm']].values
    input_class = irisdata_df['class'].values
    return (input_features, input_class)



def modeling_tests(features, labels):
    #train_feat, test_feat, train_cls, test_cls = split_dataset(features, labels)
    #decison_trees(train_feat, test_feat, train_cls, test_cls)
    #repeat_model(features, labels)

    """
    An overview on how k-folds splits the data
    Performing cross-validation, by doing many k-folds
    """
    #plot_cross_validation(labels, len(labels))
    perform_cross_validation(features, labels)







def load_data():
    '''
    No headers were provided, had to create
    '''
    irisdata_df = pd.read_csv("iris.csv",
            names=['sepal_len_cm', 'sepal_wid_cm', 'petal_len_cm', 'petal_wid_cm', 'class'], na_values=['NA'])
    return irisdata_df

def main():
    irisdata_df = load_data()
    input_feat, input_cls = store_features(irisdata_df)
    modeling_tests(input_feat, input_cls)




if __name__ == "__main__":
    main()
