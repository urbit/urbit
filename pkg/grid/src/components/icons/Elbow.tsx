import React, { HTMLAttributes } from 'react';

type ElbowProps = HTMLAttributes<SVGSVGElement>;

export const Elbow = (props: ElbowProps) => (
  <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" {...props}>
    <path
      d="M11 1V5C11 9.41828 14.5817 13 19 13H23"
      className="stroke-current"
      strokeWidth="2"
      strokeLinecap="round"
    />
  </svg>
);
