#!/usr/bin/env python3
"""
Setup script for Starkmann Email to Excel Processor
"""

from setuptools import setup, find_packages

setup(
    name="starkmann-email-processor",
    version="1.0.0",
    description="GUI application to convert Starkmann email files to Excel format",
    author="Emmanuele Somma",
    author_email="emmanuele@exedre.org",
    url="https://github.com/exedre/strkm",
    python_requires=">=3.7",
    install_requires=[
        "openpyxl>=2.6.0",
    ],
    py_modules=["starkmann_email_processor"],
    entry_points={
        "console_scripts": [
            "starkmann-processor=starkmann_email_processor:main",
        ],
    },
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Environment :: X11 Applications :: Gtk",
        "Environment :: Win32 (MS Windows)",
        "Intended Audience :: End Users/Desktop",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Office/Business",
        "Topic :: Utilities",
    ],
)
