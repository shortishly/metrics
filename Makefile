#-*- mode: makefile-gmake -*-
# Copyright (c) 2022 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = metrics
PROJECT_DESCRIPTION = Metrics
PROJECT_VERSION = 0.1.0

RELX_TAR = 0

DEPS = \
	cowboy \
	envy \
	recon

SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT)_app ensure_all_started \
	-s sync

SHELL_DEPS = \
	sync

BUILD_DEPS = elvis_mk

DEP_PLUGINS = elvis_mk

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_envy = git https://github.com/shortishly/envy.git

dep_cowboy_commit = 2.9.0
dep_envy_commit = 0.4.0
dep_recon_commit = 2.5.2


include erlang.mk

all:: elvis dialyze eunit
