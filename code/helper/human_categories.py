#!/usr/bin/env python
"""human_categories.py

Code to define the class that deals with the specifics
of the 16 categories used in Robert's human and DNN
experiments.

"""

import numpy as np
import os

import helper.wordnet_functions as wf


def compute_imagenet_indices_for_category(category):
    """Return list of ImageNet indices that correspond to category.

    'category' is part of the 16 classes.
    """
    assert category in get_human_object_recognition_categories()

    categories = HumanCategories()

    indices = []
    for i in range(0, 1000):
        WNID = wf.get_WNID_from_index(i)
        if categories.get_human_category_from_WNID(WNID) == category:
            indices.append(i)
    return indices


def get_human_object_recognition_categories():
    """Return the 16 categories that are used for the human experiment.
 
    To be more precise, return the categories that Robert uses in his
    object recognition experiment.
    """

    return sorted(["knife", "keyboard", "elephant", "bicycle", "airplane",
            "clock", "oven", "chair", "bear", "boat", "cat",
            "bottle", "truck", "car", "bird", "dog"])


def get_num_human_categories():
    """Return number of categories used in the object recogn. experiment."""

    return len(get_human_object_recognition_categories())


class HumanCategories(object):

    #Note: Some WNIDs may not be part of the ilsvrc2012 database.

    # Those WNIDs were generated with:
    # wordnet_functions.get_ilsvrc2012_training_WNID("knife") etc.
    # Since this takes some time, they were collected here for
    # a massive speed-up in computation time.
    # Excluded categories were then removed manually.

    knife =    ['n03041632']

    keyboard = ['n03085013', 'n04505470']

    elephant = ['n02504013', 'n02504458']

    bicycle =  ['n02835271', 'n03792782']

    airplane = ['n02690373', 'n03955296', 'n13861050',
                'n13941806']

    clock =    ['n02708093', 'n03196217', 'n04548280']

    oven =     ['n03259401', 'n04111414', 'n04111531']

    chair =    ['n02791124', 'n03376595', 'n04099969',
                'n00605023', 'n04429376']

    bear =     ['n02132136', 'n02133161', 'n02134084',
                'n02134418']

    boat =     ['n02951358', 'n03344393', 'n03662601',
                'n04273569', 'n04612373', 'n04612504']

    cat =      ["n02122878", "n02123045", "n02123159",
                "n02126465", "n02123394", "n02123597",
                "n02124075", "n02125311"]

    bottle =   ['n02823428', 'n03937543', 'n03983396',
                'n04557648', 'n04560804', 'n04579145',
                'n04591713']

    truck =    ['n03345487', 'n03417042', 'n03770679',
                'n03796401', 'n00319176', 'n01016201',
                'n03930630', 'n03930777', 'n05061003',
                'n06547832', 'n10432053', 'n03977966',
                'n04461696', 'n04467665']

    car =      ['n02814533', 'n03100240', 'n03100346',
                'n13419325', 'n04285008']

    bird =     ['n01321123', 'n01514859', 'n01792640',
                'n07646067', 'n01530575', 'n01531178', 'n01532829',
                'n01534433', 'n01537544', 'n01558993', 'n01562265',
                'n01560419', 'n01582220', 'n10281276', 'n01592084',
                'n01601694', 'n01614925', 'n01616318', 'n01622779',
                'n01795545', 'n01796340', 'n01797886', 'n01798484',
                'n01817953', 'n01818515', 'n01819313', 'n01820546',
                'n01824575', 'n01828970', 'n01829413', 'n01833805',
                'n01843065', 'n01843383', 'n01855032', 'n01855672',
                'n07646821', 'n01860187', 'n02002556', 'n02002724',
                'n02006656', 'n02007558', 'n02009229', 'n02009912',
                'n02011460', 'n02013706', 'n02017213', 'n02018207',
                'n02018795', 'n02025239', 'n02027492', 'n02028035',
                'n02033041', 'n02037110', 'n02051845', 'n02056570']

    dog =      ['n02085782', 'n02085936', 'n02086079',
                'n02086240', 'n02086646', 'n02086910', 'n02087046',
                'n02087394', 'n02088094', 'n02088238', 'n02088364',
                'n02088466', 'n02088632', 'n02089078', 'n02089867',
                'n02089973', 'n02090379', 'n02090622', 'n02090721',
                'n02091032', 'n02091134', 'n02091244', 'n02091467',
                'n02091635', 'n02091831', 'n02092002', 'n02092339',
                'n02093256', 'n02093428', 'n02093647', 'n02093754',
                'n02093859', 'n02093991', 'n02094114', 'n02094258',
                'n02094433', 'n02095314', 'n02095570', 'n02095889',
                'n02096051', 'n02096294', 'n02096437', 'n02096585',
                'n02097047', 'n02097130', 'n02097209', 'n02097298',
                'n02097474', 'n02097658', 'n02098105', 'n02098286',
                'n02099267', 'n02099429', 'n02099601', 'n02099712',
                'n02099849', 'n02100236', 'n02100583', 'n02100735',
                'n02100877', 'n02101006', 'n02101388', 'n02101556',
                'n02102040', 'n02102177', 'n02102318', 'n02102480',
                'n02102973', 'n02104029', 'n02104365', 'n02105056',
                'n02105162', 'n02105251', 'n02105505', 'n02105641',
                'n02105855', 'n02106030', 'n02106166', 'n02106382',
                'n02106550', 'n02106662', 'n02107142', 'n02107312',
                'n02107574', 'n02107683', 'n02107908', 'n02108000',
                'n02108422', 'n02108551', 'n02108915', 'n02109047',
                'n02109525', 'n02109961', 'n02110063', 'n02110185',
                'n02110627', 'n02110806', 'n02110958', 'n02111129',
                'n02111277', 'n08825211', 'n02111500', 'n02112018',
                'n02112350', 'n02112706', 'n02113023', 'n02113624',
                'n02113712', 'n02113799', 'n02113978']

    airplane_indices = [404]
    bear_indices = [294, 295, 296, 297]
    bicycle_indices = [444, 671]
    bird_indices = [8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 22, 23,
                    24, 80, 81, 82, 83, 87, 88, 89, 90, 91, 92, 93,
                    94, 95, 96, 98, 99, 100, 127, 128, 129, 130, 131,
                    132, 133, 135, 136, 137, 138, 139, 140, 141, 142,
                    143, 144, 145]
    boat_indices = [472, 554, 625, 814, 914]
    bottle_indices = [440, 720, 737, 898, 899, 901, 907]
    car_indices = [436, 511, 817]
    cat_indices = [281, 282, 283, 284, 285, 286]
    chair_indices = [423, 559, 765, 857]
    clock_indices = [409, 530, 892]
    dog_indices = [152, 153, 154, 155, 156, 157, 158, 159, 160, 161,
                   162, 163, 164, 165, 166, 167, 168, 169, 170, 171,
                   172, 173, 174, 175, 176, 177, 178, 179, 180, 181,
                   182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
                   193, 194, 195, 196, 197, 198, 199, 200, 201, 202,
                   203, 205, 206, 207, 208, 209, 210, 211, 212, 213,
                   214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
                   224, 225, 226, 228, 229, 230, 231, 232, 233, 234,
                   235, 236, 237, 238, 239, 240, 241, 243, 244, 245,
                   246, 247, 248, 249, 250, 252, 253, 254, 255, 256,
                   257, 259, 261, 262, 263, 265, 266, 267, 268]
    elephant_indices = [385, 386] 
    keyboard_indices = [508, 878]
    knife_indices = [499]
    oven_indices = [766]
    truck_indices = [555, 569, 656, 675, 717, 734, 864, 867]


    def get_human_category_from_WNID(self, wnid):
        """Return the MS COCO category for a given WNID.

        Returns None if wnid is not part of the 16 human categories.

        parameters:
        - wnid: a string containing the wnid of an image, e.g. 'n03658185'

        """
        
        categories = get_human_object_recognition_categories()
        for c in categories:
            attr = getattr(self, c)
            if wnid in attr:
                return c

        return None

    def get_imagenet_indices_for_category(self, category):
        """Return ImageNet indices that correspond to an entry-level category.

        Returns error if 'category' is not part of the 16 human categories.

        parameters:
        - category: a string, e.g. "dog" or "knife"
        """

        assert category in get_human_object_recognition_categories()

        return getattr(self, category+"_indices")


