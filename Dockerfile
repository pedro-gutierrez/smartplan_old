FROM erlang:18
RUN mkdir /build
WORKDIR /build
ENV CODE_LOADING_MODE interactive
ENV RELX_REPLACE_OS_VARS true
RUN git clone https://github.com/rvirding/lfe.git
WORKDIR /build/lfe
RUN make compile; make install
WORKDIR /build
RUN git clone https://github.com/pedro-gutierrez/smartplan.git
WORKDIR /build/smartplan
RUN wget https://github.com/rebar/rebar/wiki/rebar; chmod +x rebar; make deps; 
RUN make compile; make release
CMD ["make", "run"]
