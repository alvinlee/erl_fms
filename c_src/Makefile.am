pkglibdir = $(ERLANG_INSTALL_LIB_DIR_erlria)/priv
pkglib_LTLIBRARIES = erlria_drv.la
erlria_drv_la_CPPFLAGS = -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ -DDEBUG
erlria_drv_la_LDFLAGS = -module -lcrypto
erlria_drv_la_SOURCES = erlria_drv.c
# liberlria_la_LDFLAGS = $(ERLANG_INSTALL_LIB_DIR_erlria)/priv

