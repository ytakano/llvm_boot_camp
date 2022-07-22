FROM ubuntu:latest

RUN apt-get -y update && apt-get -y upgrade && apt-get install -y wget git gnupg2 cmake build-essential

RUN cd /tmp && wget https://apt.llvm.org/llvm-snapshot.gpg.key && apt-key add /tmp/llvm-snapshot.gpg.key
RUN apt-get -y update && apt-get -y install clang-13 lldb-13 lld-13 libllvm-13-ocaml-dev libllvm13 llvm-13 llvm-13-dev llvm-13-doc llvm-13-examples llvm-13-runtime

RUN echo "export CC=/usr/bin/clang-13" >> /root/.bashrc
RUN echo "export CXX=/usr/bin/clang++-13" >> /root/.bashrc

CMD ["/bin/bash"]
