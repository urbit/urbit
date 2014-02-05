#ifndef CAPN_A727C25CC7018F2
#define CAPN_A727C25CC7018F2
/* AUTO GENERATED - DO NOT EDIT */
#include <capn.h>

#if CAPN_VERSION != 1
#error "version mismatch between capn.h and generated code"
#endif


#ifdef __cplusplus
extern "C" {
#endif

struct Raft;
struct Raft_Comd;
struct Raft_Rent;
struct Raft_Apen;
struct Raft_Revo;
struct Raft_Rasp;

typedef struct {capn_ptr p;} Raft_ptr;
typedef struct {capn_ptr p;} Raft_Comd_ptr;
typedef struct {capn_ptr p;} Raft_Rent_ptr;
typedef struct {capn_ptr p;} Raft_Apen_ptr;
typedef struct {capn_ptr p;} Raft_Revo_ptr;
typedef struct {capn_ptr p;} Raft_Rasp_ptr;

typedef struct {capn_ptr p;} Raft_list;
typedef struct {capn_ptr p;} Raft_Comd_list;
typedef struct {capn_ptr p;} Raft_Rent_list;
typedef struct {capn_ptr p;} Raft_Apen_list;
typedef struct {capn_ptr p;} Raft_Revo_list;
typedef struct {capn_ptr p;} Raft_Rasp_list;

enum Raft_Comd_Type {
	Raft_Comd_Type_nop = 0,
	Raft_Comd_Type_ova = 1
};

struct Raft {
};

struct Raft_Comd {
	enum Raft_Comd_Type typ;
	capn_data bob;
};

struct Raft_Rent {
	uint64_t tem;
	Raft_Comd_ptr cmd;
};

struct Raft_Apen {
	uint64_t tem;
	capn_text cid;
	uint64_t lai;
	uint64_t lat;
	Raft_Rent_list ent;
	uint64_t cit;
};

struct Raft_Revo {
	uint64_t tem;
	capn_text cid;
	uint64_t lai;
	uint64_t lat;
};

struct Raft_Rasp {
	uint64_t tem;
	unsigned suc : 1;
};

Raft_ptr new_Raft(struct capn_segment*);
Raft_Comd_ptr new_Raft_Comd(struct capn_segment*);
Raft_Rent_ptr new_Raft_Rent(struct capn_segment*);
Raft_Apen_ptr new_Raft_Apen(struct capn_segment*);
Raft_Revo_ptr new_Raft_Revo(struct capn_segment*);
Raft_Rasp_ptr new_Raft_Rasp(struct capn_segment*);

Raft_list new_Raft_list(struct capn_segment*, int len);
Raft_Comd_list new_Raft_Comd_list(struct capn_segment*, int len);
Raft_Rent_list new_Raft_Rent_list(struct capn_segment*, int len);
Raft_Apen_list new_Raft_Apen_list(struct capn_segment*, int len);
Raft_Revo_list new_Raft_Revo_list(struct capn_segment*, int len);
Raft_Rasp_list new_Raft_Rasp_list(struct capn_segment*, int len);

void read_Raft(struct Raft*, Raft_ptr);
void read_Raft_Comd(struct Raft_Comd*, Raft_Comd_ptr);
void read_Raft_Rent(struct Raft_Rent*, Raft_Rent_ptr);
void read_Raft_Apen(struct Raft_Apen*, Raft_Apen_ptr);
void read_Raft_Revo(struct Raft_Revo*, Raft_Revo_ptr);
void read_Raft_Rasp(struct Raft_Rasp*, Raft_Rasp_ptr);

void write_Raft(const struct Raft*, Raft_ptr);
void write_Raft_Comd(const struct Raft_Comd*, Raft_Comd_ptr);
void write_Raft_Rent(const struct Raft_Rent*, Raft_Rent_ptr);
void write_Raft_Apen(const struct Raft_Apen*, Raft_Apen_ptr);
void write_Raft_Revo(const struct Raft_Revo*, Raft_Revo_ptr);
void write_Raft_Rasp(const struct Raft_Rasp*, Raft_Rasp_ptr);

void get_Raft(struct Raft*, Raft_list, int i);
void get_Raft_Comd(struct Raft_Comd*, Raft_Comd_list, int i);
void get_Raft_Rent(struct Raft_Rent*, Raft_Rent_list, int i);
void get_Raft_Apen(struct Raft_Apen*, Raft_Apen_list, int i);
void get_Raft_Revo(struct Raft_Revo*, Raft_Revo_list, int i);
void get_Raft_Rasp(struct Raft_Rasp*, Raft_Rasp_list, int i);

void set_Raft(const struct Raft*, Raft_list, int i);
void set_Raft_Comd(const struct Raft_Comd*, Raft_Comd_list, int i);
void set_Raft_Rent(const struct Raft_Rent*, Raft_Rent_list, int i);
void set_Raft_Apen(const struct Raft_Apen*, Raft_Apen_list, int i);
void set_Raft_Revo(const struct Raft_Revo*, Raft_Revo_list, int i);
void set_Raft_Rasp(const struct Raft_Rasp*, Raft_Rasp_list, int i);

#ifdef __cplusplus
}
#endif
#endif
