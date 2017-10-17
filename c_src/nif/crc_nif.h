// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#ifndef CRC_NIF_H
#define CRC_NIF_H

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int erts_fprintf(FILE *, const char *, ...);

#ifdef __cplusplus
}
#endif

// #define TRACE 1
#ifdef TRACE
#ifndef TRACE_C
#define TRACE_C(c)                                                                                                                 \
    do {                                                                                                                           \
        putchar(c);                                                                                                                \
        fflush(stdout);                                                                                                            \
    } while (0)
#endif
#ifndef TRACE_S
#define TRACE_S(s)                                                                                                                 \
    do {                                                                                                                           \
        fputs((s), stdout);                                                                                                        \
        fflush(stdout);                                                                                                            \
    } while (0)
#endif
#ifndef TRACE_F
#define TRACE_F(...)                                                                                                               \
    do {                                                                                                                           \
        erts_fprintf(stderr, "%p ", (void *)enif_thread_self());                                                                   \
        erts_fprintf(stderr, __VA_ARGS__);                                                                                         \
        fflush(stderr);                                                                                                            \
    } while (0)
#endif
#else
#ifndef TRACE_C
#define TRACE_C(c) ((void)(0))
#endif
#ifndef TRACE_S
#define TRACE_S(s) ((void)(0))
#endif
#ifndef TRACE_F
#define TRACE_F(...) ((void)(0))
#endif
#endif

#endif
