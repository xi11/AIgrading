import os
import numpy as np
import argparse
import tensorflow as tf
import random as rn
from pandas import DataFrame
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import LearningRateScheduler
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from Self3CrossAttention import selfCrossPsp

np.random.seed(2021)
tf.random.set_seed(2021)
rn.seed(2021)

if os.name == 'nt':
    os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"
    os.environ["CUDA_VISIBLE_DEVICES"] = "1"

def scheduler(epoch, lr):
    if epoch <10:
        print("lr changed to {}".format(lr))
        return lr

    elif epoch >= 10 and epoch <50:
        print("lr changed to {}".format(0.0001))
        return 0.0001

    else:
        print("lr changed to {}".format(0.00001))
        return 0.00001

def random_adjust_saturation(image, min_delta=0.8, max_delta=2.0, max_delta_hue=0.1,seed=None):
    delta = tf.random.uniform([], -max_delta_hue, max_delta_hue, seed=seed)
    image = tf.image.adjust_hue(image / 255.0, delta)
    image = tf.clip_by_value(image, clip_value_min=0.0, clip_value_max=1.0)
    saturation_factor = tf.random.uniform([], min_delta, max_delta, seed=seed)
    image = tf.image.adjust_saturation(image, saturation_factor)
    image = tf.clip_by_value(image, clip_value_min=0.0, clip_value_max=1.0)
    return image

def train_generator(image_generator_c, mask_generator):
    while True:
        yield (image_generator_c.next(), mask_generator.next())

def train_model(input_dir, target_dir, img_size, num_class, batch_size, num_epoch):
    input_img_paths = sorted(
        [
            os.path.join(input_dir, fname)
            for fname in os.listdir(input_dir)
            if fname.endswith(".png")
        ]
    )

    target_img_paths = sorted(
        [
            os.path.join(target_dir, fname)
            for fname in os.listdir(target_dir)
            if fname.endswith(".png")
        ]
    )
    print("Number of samples:", len(input_img_paths))

    img = keras.Input(shape=(img_size, img_size, 3))
    o = selfCrossPsp(num_class, img)
    o = (layers.Activation('softmax'))(o)
    model = keras.Model(img, o)
    #model.summary()
    train_samples = len(input_img_paths)

    rn.Random(2021).shuffle(input_img_paths)
    rn.Random(2021).shuffle(target_img_paths)
    train_input_img_paths = input_img_paths
    train_target_img_paths = target_img_paths

    df_train = DataFrame(train_input_img_paths,columns=['filename'])
    df_train_target = DataFrame(train_target_img_paths,columns=['filename'])
    data_gen_args = dict(
                         rotation_range=90,
                         width_shift_range=0.2,
                         height_shift_range=0.2,
                         zoom_range=0.2,
                         fill_mode='constant',
                         preprocessing_function=random_adjust_saturation)

    data_gen_args_mask = dict(rescale=1.,
                         rotation_range=90,
                         width_shift_range=0.2,
                         height_shift_range=0.2,
                         zoom_range=0.2,
                         fill_mode='constant'
                         )

    image_datagen = ImageDataGenerator(**data_gen_args)
    mask_datagen = ImageDataGenerator(**data_gen_args_mask)
    seed = 2021
    image_generator = image_datagen.flow_from_dataframe(
        df_train,
        target_size=(img_size, img_size),
        class_mode=None,
        batch_size=batch_size,
        seed=seed)

    mask_generator = mask_datagen.flow_from_dataframe(
        df_train_target,
        target_size=(img_size, img_size),
        color_mode='grayscale',
        class_mode=None,
        batch_size=batch_size,
        seed=seed)

    adam = Adam(lr=0.001)
    model.compile(optimizer=adam, loss=['sparse_categorical_crossentropy'], metrics=['sparse_categorical_accuracy'])
    callbacks = [LearningRateScheduler(scheduler)]
    model.fit(train_generator(image_generator, mask_generator),
        steps_per_epoch=int(np.ceil(train_samples/batch_size)),
        epochs=num_epoch, verbose=1, callbacks=callbacks)

    modelpath = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'models')
    if not os.path.exists(modelpath):
        os.makedirs(modelpath)
    model.save(os.path.join(modelpath, 'AIgrading_anorak.h5'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--input_dir', type=str, help='path to the input directory')
    parser.add_argument('--target_dir', type=str, help='path to the target directory')
    parser.add_argument('--img_size', type=int, help='image size')
    parser.add_argument('--num_class', type=int, help='number of classes')
    parser.add_argument('--batch_size', type=int, help='batch size')
    parser.add_argument('--num_epoch', type=int, help='number of epochs')
    args = parser.parse_args()
    train_model(args.input_dir, args.target_dir, args.img_size, args.num_class, args.batch_size, args.num_epoch)
