# -------------------------------------------------------------------
#
# Copyright (c) 2015 Basho Technologies, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# -------------------------------------------------------------------

REBAR	?= rebar
PRJDIR	:= $(shell cd $(dir $(lastword $(MAKEFILE_LIST))) && pwd)
#
# Test source code uses this same pattern to find the PLT file, so changing
# it WILL break things!
#
OTPVSN	:= $(shell erl -noshell -eval \
		'io:fwrite("~s",[erlang:system_info(otp_release)]), halt().')
PLTFILE	:= $(PRJDIR)/dialyzer_$(OTPVSN).plt
PLTAPPS	:= erts kernel stdlib compiler

DZARGS	:= --verbose -Wunmatched_returns -Werror_handling -Wrace_conditions
DZSARGS	:= --src -I $(PRJDIR)/include -DDIALYZER
ifeq (,$(findstring R,$(OTPVSN)))
DZSARGS	+= -Dnamespaced_types
endif

# Default EDoc stylesheet
CSSFILE := $(PRJDIR)/doc/stylesheet.css
# Include an ugly hack to bump up the monospaced font size, because most
# browsers seem to set it a few points smaller than the body text size.
# If your browser handles it appropriately, just change it to normal.
CSSADDL := code,kbd,pre,tt { font-size: larger; }

compile ::
	$(REBAR) compile

clean ::
	$(REBAR) clean

test ::
	$(REBAR) eunit

check :: dialyzer

checks :: dialyzers

docs ::
	$(REBAR) skip_deps=true doc
	@grep -q '$(CSSADDL)' $(CSSFILE) || echo '$(CSSADDL)' >> $(CSSFILE)
	@echo Doc entry point: $(PRJDIR)/doc/index.html

dialyzer :: compile $(PLTFILE)
	dialyzer --plt $(PLTFILE) $(DZARGS) ebin

dialyzers :: $(PLTFILE)
	dialyzer --plt $(PLTFILE) $(DZSARGS) $(DZARGS) src

$(PLTFILE) :
	dialyzer --build_plt --output_plt $(PLTFILE) --apps $(PLTAPPS)

