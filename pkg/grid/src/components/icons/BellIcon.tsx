import React from 'react';

export default function BellIcon(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg {...props} viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M6 11a6.002 6.002 0 0 1 5-5.917V3h2v2.083c2.838.476 5 2.944 5 5.917v2.382l.764.382a2.236 2.236 0 0 1-1 4.236H15a3 3 0 1 1-6 0H6.236a2.236 2.236 0 0 1-1-4.236L6 13.382V11Zm5 7a1 1 0 1 0 2 0h-2Zm-3-7a4 4 0 1 1 8 0v2.902c0 .439.248.84.64 1.036l1.23.615a.236.236 0 0 1-.106.447H6.236a.236.236 0 0 1-.106-.447l1.23-.615c.392-.196.64-.597.64-1.036V11Z"
        className="fill-current"
      />
    </svg>
  );
}
