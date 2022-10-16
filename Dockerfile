FROM erlang:18
RUN mkdir /build
WORKDIR /build
ENV CODE_LOADING_MODE interactive
ENV RELX_REPLACE_OS_VARS true
RUN git clone https://github.com/rvirding/lfe.git; cd lfe; git checkout 17f45c99e20283f5a47336b0dbe2c683d41873ab
WORKDIR /build/lfe
RUN make compile; make install
WORKDIR /build
RUN git clone https://github.com/pedro-gutierrez/smartplan.git
WORKDIR /build/smartplan
RUN wget https://github.com/rebar/rebar/wiki/rebar; chmod +x rebar; make deps;
RUN make compile; make release
CMD ["make", "run"]
