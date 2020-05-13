# Author: mertyg

import requests
import pandas as pd
import numpy as np
import json


# Below is a sample script for your submissions.
# We advise you to play around with the code and read it carefully.
# Feel free to grab the utility functions below and experiment with them yourself.
# We want to remind you that you can submit more than once, and we will use the latest one.
# IMPORTANT: Below, you need to fill your `predict` function for your predictions
# and add your username & password in the space below.
# Set submit_now to false if you are experimenting and just want to see the result of your predictions.
# Set to True if you actually want to submit.


URL = 'http://167.172.183.67'

USERNAME = "username"
PASSWORD = "password"
submit_now = False # Set this to True if you want to submit your predictions.


def predict(data: pd.DataFrame):
    ### YOUR CODE GOES HERE
    """
    Students are expected to fill this method.
    :param data: Data that was obtained from the API.
    :return: A list of floats with length 24
    """

    products = pd.unique(data["product_content_id"])

    predictions = {}

    for product in products:
        data_product = data[data["product_content_id"] == product]
        sold = data_product["sold_count"].to_numpy()

        # Use the latest value as the trivial prediction
        predictions[product] = sold[0]

        
    ### YOUR CODE ENDS HERE
    print(predictions)  # Should be a dictionary of forecasts
    # i.e. {"id1" : forecast, "id2": forecast, ...}
    return predictions


### CODE BY THE TEACHING STAFF BEGINS HERE - YOU DO NOT NEED TO CHANGE###


def get_token(username, password):
    body = {
        "username": username,
        "password": password
    }
    r = requests.post(f'{URL}/token/', data=body)
    r = r.json()
    token = r["key"]
    return token


def get_data(token, start_date='2020-03-20'):
    # Date format : YEAR-MONTH-DAY
    header = {'Authorization': f'Token {token}'}
    r = requests.get(f'{URL}/dataset/', params={'start_date': start_date}, headers=header)
    r = r.json()
    data = pd.DataFrame.from_dict(r)
    data["event_date"] = pd.to_datetime(data["event_date"])
    data = data.sort_values(by=["event_date"])
    return data


def check_format(predictions):
    assert isinstance(predictions, dict)
    for id, pred in predictions.items():
        try:
            predictions[id] = float(pred)
        except:
            error_str = f"Your prediction = {pred} for id: {id} is not a numerical value. Please cast it to either native Python int/floats, or np.number subtype before submission."
            raise ValueError(error_str)


def send_submission(predictions: dict, token: str, submit_now: bool):
    check_format(predictions)
    submission = predictions
    print(f"Your submission will be : {submission}")

    if not submit_now:
        print("You did not submit.")
        return

    submission_body = {"submission": json.dumps(submission)}
    header = {'Authorization': f'Token {token}'}
    r = requests.post(f'{URL}/submission/', data=submission_body,
                      headers=header)

    if r.status_code == 201:
        print("Successfully submitted. Below you can see the details of your submission")

    else:
        print("Could not submit. Please check the error message below, contact the assistant if needed.")

    r = r.json()
    print(r)


if __name__ == "__main__":
    ### YOUR CODE GOES HERE
    username = USERNAME
    password = PASSWORD
    ### YOUR CODE ENDS HERE
    token = get_token(username, password)
    data = get_data(token)
    prediction = predict(data)
    send_submission(prediction, token, submit_now)

### CODE BY THE TEACHING STAFF ENDS HERE - YOU DO NOT `NEED` TO CHANGE ###
