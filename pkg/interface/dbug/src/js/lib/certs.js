//  reflect-metadata must be imported before @peculiar/x509: the library uses
//  tsyringe for its crypto-provider DI, and tsyringe throws on load without it.
import 'reflect-metadata';
import {
  X509Certificate,
  SubjectAlternativeNameExtension,
  BasicConstraintsExtension
} from '@peculiar/x509';

//  parsing is pure ASN.1, so this works on insecure origins too (plain http
//  to anything but localhost), where window.crypto.subtle is undefined.

//  parseCert: pem string -> cert details
//
export function parseCert(pem) {
  const cert = new X509Certificate(pem)
  const san = cert.getExtension(SubjectAlternativeNameExtension);
  const names = san ? san.names.items : [];

  return {
    //  the domains this cert covers, straight from the SANs. a cert with no SAN
    //  extension gets an empty list even when its CN names a domain: that's not
    //  an omission, it's what browsers see. since Chrome 58 a SAN-less cert is
    //  rejected no matter what its CN says. render commonName separately.
    domains: names.filter(n => n.type === 'dns').map(n => n.value),
    ips: names.filter(n => n.type === 'ip').map(n => n.value),
    hasSAN: !!san,
    commonName: cert.subjectName.getField('CN')[0] || null,

    validFrom: cert.notBefore,
    validTo: cert.notAfter,

    subject: cert.subject,
    issuer: cert.issuer,
    issuerName: cert.issuerName.getField('CN')[0] || null,
    serialNumber: cert.serialNumber,
    signatureAlgorithm: cert.signatureAlgorithm.name,
    isCA: !!cert.getExtension(BasicConstraintsExtension)?.ca
  };
}

//  validity: cert details -> where we are in its lifetime, as of %now
//
export function validity(deets, now = new Date()) {
  const msLeft = deets.validTo - now;
  return {
    active: now >= deets.validFrom && msLeft > 0,
    expired: msLeft <= 0,
    notYetValid: now < deets.validFrom,
    daysLeft: Math.floor(msLeft / 86400000)
  };
}
