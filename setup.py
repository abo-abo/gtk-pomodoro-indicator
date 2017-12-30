#!/usr/bin/env python

from setuptools import setup, find_packages

setup(
    name='pomodoro-indicator',
    version='0.1.2',
    description='Pomodoro timer indicator.',
    long_description='https://raw.githubusercontent.com/abo-abo/gtk-pomodoro-indicator/master/README.org',
    packages=find_packages(),
    url='https://github.com/abo-abo/gtk-pomodoro-indicator',
    author='Oleh Krehel',
    author_email='ohwoeowho@gmail.com',
    license='GPLv3+',
    keywords='pomodoro',
    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: End Users/Desktop',
        'Environment :: X11 Applications',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5'],
    install_requires=[],
    data_files=[('pomodoro-indicator', ['icons/coffee.svg', 'icons/stopwatch.svg'])],
    entry_points={'console_scripts': ['gpi=pomodoro_indicator.pomodoro_indicator:main']}
)
