#!/bin/bash

mkdir -p ebin
erl -make
erl -pa ebin -noshell -eval "hlr_test:test()" -run init stop