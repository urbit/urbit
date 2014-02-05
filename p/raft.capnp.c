#include "p/raft.capnp.h"
/* AUTO GENERATED - DO NOT EDIT */
static const capn_text capn_val0 = {0,""};

Raft_ptr new_Raft(struct capn_segment *s) {
	Raft_ptr p;
	p.p = capn_new_struct(s, 0, 0);
	return p;
}
Raft_list new_Raft_list(struct capn_segment *s, int len) {
	Raft_list p;
	p.p = capn_new_list(s, len, 0, 0);
	return p;
}
void read_Raft(struct Raft *s, Raft_ptr p) {
	capn_resolve(&p.p);
}
void write_Raft(const struct Raft *s, Raft_ptr p) {
	capn_resolve(&p.p);
}
void get_Raft(struct Raft *s, Raft_list l, int i) {
	Raft_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft(s, p);
}
void set_Raft(const struct Raft *s, Raft_list l, int i) {
	Raft_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft(s, p);
}

Raft_Comd_ptr new_Raft_Comd(struct capn_segment *s) {
	Raft_Comd_ptr p;
	p.p = capn_new_struct(s, 8, 1);
	return p;
}
Raft_Comd_list new_Raft_Comd_list(struct capn_segment *s, int len) {
	Raft_Comd_list p;
	p.p = capn_new_list(s, len, 8, 1);
	return p;
}
void read_Raft_Comd(struct Raft_Comd *s, Raft_Comd_ptr p) {
	capn_resolve(&p.p);
	s->typ = (enum Raft_Comd_Type) capn_read16(p.p, 0);
	s->bob = capn_get_data(p.p, 0);
}
void write_Raft_Comd(const struct Raft_Comd *s, Raft_Comd_ptr p) {
	capn_resolve(&p.p);
	capn_write16(p.p, 0, (uint16_t) s->typ);
	capn_setp(p.p, 0, s->bob.p);
}
void get_Raft_Comd(struct Raft_Comd *s, Raft_Comd_list l, int i) {
	Raft_Comd_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Comd(s, p);
}
void set_Raft_Comd(const struct Raft_Comd *s, Raft_Comd_list l, int i) {
	Raft_Comd_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Comd(s, p);
}

Raft_Rent_ptr new_Raft_Rent(struct capn_segment *s) {
	Raft_Rent_ptr p;
	p.p = capn_new_struct(s, 8, 1);
	return p;
}
Raft_Rent_list new_Raft_Rent_list(struct capn_segment *s, int len) {
	Raft_Rent_list p;
	p.p = capn_new_list(s, len, 8, 1);
	return p;
}
void read_Raft_Rent(struct Raft_Rent *s, Raft_Rent_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read64(p.p, 0);
	s->cmd.p = capn_getp(p.p, 0, 0);
}
void write_Raft_Rent(const struct Raft_Rent *s, Raft_Rent_ptr p) {
	capn_resolve(&p.p);
	capn_write64(p.p, 0, s->tem);
	capn_setp(p.p, 0, s->cmd.p);
}
void get_Raft_Rent(struct Raft_Rent *s, Raft_Rent_list l, int i) {
	Raft_Rent_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rent(s, p);
}
void set_Raft_Rent(const struct Raft_Rent *s, Raft_Rent_list l, int i) {
	Raft_Rent_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rent(s, p);
}

Raft_Apen_ptr new_Raft_Apen(struct capn_segment *s) {
	Raft_Apen_ptr p;
	p.p = capn_new_struct(s, 32, 2);
	return p;
}
Raft_Apen_list new_Raft_Apen_list(struct capn_segment *s, int len) {
	Raft_Apen_list p;
	p.p = capn_new_list(s, len, 32, 2);
	return p;
}
void read_Raft_Apen(struct Raft_Apen *s, Raft_Apen_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read64(p.p, 0);
	s->cid = capn_get_text(p.p, 0, capn_val0);
	s->lai = capn_read64(p.p, 8);
	s->lat = capn_read64(p.p, 16);
	s->ent.p = capn_getp(p.p, 1, 0);
	s->cit = capn_read64(p.p, 24);
}
void write_Raft_Apen(const struct Raft_Apen *s, Raft_Apen_ptr p) {
	capn_resolve(&p.p);
	capn_write64(p.p, 0, s->tem);
	capn_set_text(p.p, 0, s->cid);
	capn_write64(p.p, 8, s->lai);
	capn_write64(p.p, 16, s->lat);
	capn_setp(p.p, 1, s->ent.p);
	capn_write64(p.p, 24, s->cit);
}
void get_Raft_Apen(struct Raft_Apen *s, Raft_Apen_list l, int i) {
	Raft_Apen_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Apen(s, p);
}
void set_Raft_Apen(const struct Raft_Apen *s, Raft_Apen_list l, int i) {
	Raft_Apen_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Apen(s, p);
}

Raft_Revo_ptr new_Raft_Revo(struct capn_segment *s) {
	Raft_Revo_ptr p;
	p.p = capn_new_struct(s, 24, 1);
	return p;
}
Raft_Revo_list new_Raft_Revo_list(struct capn_segment *s, int len) {
	Raft_Revo_list p;
	p.p = capn_new_list(s, len, 24, 1);
	return p;
}
void read_Raft_Revo(struct Raft_Revo *s, Raft_Revo_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read64(p.p, 0);
	s->cid = capn_get_text(p.p, 0, capn_val0);
	s->lai = capn_read64(p.p, 8);
	s->lat = capn_read64(p.p, 16);
}
void write_Raft_Revo(const struct Raft_Revo *s, Raft_Revo_ptr p) {
	capn_resolve(&p.p);
	capn_write64(p.p, 0, s->tem);
	capn_set_text(p.p, 0, s->cid);
	capn_write64(p.p, 8, s->lai);
	capn_write64(p.p, 16, s->lat);
}
void get_Raft_Revo(struct Raft_Revo *s, Raft_Revo_list l, int i) {
	Raft_Revo_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Revo(s, p);
}
void set_Raft_Revo(const struct Raft_Revo *s, Raft_Revo_list l, int i) {
	Raft_Revo_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Revo(s, p);
}

Raft_Rasp_ptr new_Raft_Rasp(struct capn_segment *s) {
	Raft_Rasp_ptr p;
	p.p = capn_new_struct(s, 16, 0);
	return p;
}
Raft_Rasp_list new_Raft_Rasp_list(struct capn_segment *s, int len) {
	Raft_Rasp_list p;
	p.p = capn_new_list(s, len, 16, 0);
	return p;
}
void read_Raft_Rasp(struct Raft_Rasp *s, Raft_Rasp_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read64(p.p, 0);
	s->suc = (capn_read8(p.p, 8) & 1) != 0;
}
void write_Raft_Rasp(const struct Raft_Rasp *s, Raft_Rasp_ptr p) {
	capn_resolve(&p.p);
	capn_write64(p.p, 0, s->tem);
	capn_write1(p.p, 64, s->suc != 0);
}
void get_Raft_Rasp(struct Raft_Rasp *s, Raft_Rasp_list l, int i) {
	Raft_Rasp_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rasp(s, p);
}
void set_Raft_Rasp(const struct Raft_Rasp *s, Raft_Rasp_list l, int i) {
	Raft_Rasp_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rasp(s, p);
}
