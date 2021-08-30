import React from 'react';

export const Bullet = (props: React.SVGProps<SVGSVGElement>) => (
  <svg {...props} viewBox="0 0 16 16">
    <circle className="fill-current" cx="8" cy="8" r="3" />
  </svg>
);
