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

DEPS += cowboy
DEPS += envy
DEPS += recon

SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

SHELL_DEPS += sync

dep_envy = $(if $(DEP_LN),ln ../../envy,git https://github.com/shortishly/envy.git)

dep_cowboy_commit = 2.10.0
dep_envy_commit = 0.9.2
dep_recon_commit = 2.5.4

PLT_APPS += any
PLT_APPS += asn1
PLT_APPS += compiler
PLT_APPS += cowlib
PLT_APPS += crypto
PLT_APPS += inets
PLT_APPS += mnesia
PLT_APPS += public_key
PLT_APPS += ranch
PLT_APPS += runtime_tools
PLT_APPS += ssl
PLT_APPS += stdlib
PLT_APPS += syntax_tools
PLT_APPS += tools


include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

app:: rebar.config
