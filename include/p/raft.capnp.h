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
struct Raft_Rest;
struct Raft_Rasp;

typedef struct {capn_ptr p;} Raft_ptr;
typedef struct {capn_ptr p;} Raft_Comd_ptr;
typedef struct {capn_ptr p;} Raft_Rent_ptr;
typedef struct {capn_ptr p;} Raft_Rest_ptr;
typedef struct {capn_ptr p;} Raft_Rasp_ptr;

typedef struct {capn_ptr p;} Raft_list;
typedef struct {capn_ptr p;} Raft_Comd_list;
typedef struct {capn_ptr p;} Raft_Rent_list;
typedef struct {capn_ptr p;} Raft_Rest_list;
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
	uint32_t tem;
	Raft_Comd_ptr cmd;
};
enum Raft_Rest_which {
	Raft_Rest_revo = 0,
	Raft_Rest_apen = 1
};

struct Raft_Rest {
	uint32_t tem;
	capn_text cid;
	uint64_t lai;
	uint32_t lat;
	enum Raft_Rest_which which;
	union {
		struct {
			Raft_Rent_list ent;
			uint64_t cit;
		} apen;
	};
};

struct Raft_Rasp {
	uint64_t tem;
	unsigned suc : 1;
};

Raft_ptr new_Raft(struct capn_segment*);
Raft_Comd_ptr new_Raft_Comd(struct capn_segment*);
Raft_Rent_ptr new_Raft_Rent(struct capn_segment*);
Raft_Rest_ptr new_Raft_Rest(struct capn_segment*);
Raft_Rasp_ptr new_Raft_Rasp(struct capn_segment*);

Raft_list new_Raft_list(struct capn_segment*, int len);
Raft_Comd_list new_Raft_Comd_list(struct capn_segment*, int len);
Raft_Rent_list new_Raft_Rent_list(struct capn_segment*, int len);
Raft_Rest_list new_Raft_Rest_list(struct capn_segment*, int len);
Raft_Rasp_list new_Raft_Rasp_list(struct capn_segment*, int len);

void read_Raft(struct Raft*, Raft_ptr);
void read_Raft_Comd(struct Raft_Comd*, Raft_Comd_ptr);
void read_Raft_Rent(struct Raft_Rent*, Raft_Rent_ptr);
void read_Raft_Rest(struct Raft_Rest*, Raft_Rest_ptr);
void read_Raft_Rasp(struct Raft_Rasp*, Raft_Rasp_ptr);

void write_Raft(const struct Raft*, Raft_ptr);
void write_Raft_Comd(const struct Raft_Comd*, Raft_Comd_ptr);
void write_Raft_Rent(const struct Raft_Rent*, Raft_Rent_ptr);
void write_Raft_Rest(const struct Raft_Rest*, Raft_Rest_ptr);
void write_Raft_Rasp(const struct Raft_Rasp*, Raft_Rasp_ptr);

void get_Raft(struct Raft*, Raft_list, int i);
void get_Raft_Comd(struct Raft_Comd*, Raft_Comd_list, int i);
void get_Raft_Rent(struct Raft_Rent*, Raft_Rent_list, int i);
void get_Raft_Rest(struct Raft_Rest*, Raft_Rest_list, int i);
void get_Raft_Rasp(struct Raft_Rasp*, Raft_Rasp_list, int i);

void set_Raft(const struct Raft*, Raft_list, int i);
void set_Raft_Comd(const struct Raft_Comd*, Raft_Comd_list, int i);
void set_Raft_Rent(const struct Raft_Rent*, Raft_Rent_list, int i);
void set_Raft_Rest(const struct Raft_Rest*, Raft_Rest_list, int i);
void set_Raft_Rasp(const struct Raft_Rasp*, Raft_Rasp_list, int i);

#ifdef __cplusplus
}
#endif
#endif
