#!/usr/bin/env python

import numpy as np
from abc import ABC, abstractmethod

import helper.human_categories as hc


class ProbabilitiesToDecisionMapping(ABC):

    @abstractmethod
    def probabilities_to_decision(self, probabilities):
        pass


    def check_input(self, probabilities):
        """Run assert checks on probabilities.

        Keyword arguments:
        probabilities -- a np.ndarray of length 1000
                         (softmax output: all values should be
                         within [0,1])
        """


        assert type(probabilities) is np.ndarray
        assert (probabilities >= 0.0).all() and (probabilities <= 1.0).all()


class ImageNetProbabilitiesTo16ClassesMapping(ProbabilitiesToDecisionMapping):
    """Return the entry-level category decision for probabilities."""

    def __init__(self, aggregation_function=np.mean):

        self.aggregation_function = aggregation_function


    def probabilities_to_decision(self, probabilities):
        """Return one of 16 categories for vector of probabilities.

        Keyword arguments:
        probabilities -- a np.ndarray of length 1000
                         (softmax output: all values should be
                         within [0,1])
        """

        self.check_input(probabilities)
        assert len(probabilities) == 1000

        max_value = -float("inf")
        category_decision = None
        c = hc.HumanCategories()
        for category in hc.get_human_object_recognition_categories():
            indices = c.get_imagenet_indices_for_category(category)
            values = np.take(probabilities, indices)
            aggregated_value = self.aggregation_function(values)
            if aggregated_value > max_value:
                max_value = aggregated_value
                category_decision = category
   
        return category_decision

