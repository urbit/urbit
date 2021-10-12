import React, { HTMLAttributes } from 'react';

type LeftArrowProps = HTMLAttributes<SVGSVGElement>;

export const LeftArrow = (props: LeftArrowProps) => (
  <svg viewBox="0 0 17 17" fill="none" xmlns="http://www.w3.org/2000/svg" {...props}>
    <path
      d="M16 8.5H1M1 8.5L7.5 2M1 8.5L7.5 15"
      className="stroke-current"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);
