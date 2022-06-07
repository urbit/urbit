import React from 'react';

export const Lock = (props: React.SVGProps<SVGSVGElement>) => (
  <svg viewBox="-8.5 -6.5 26 26" fill="none" xmlns="http://www.w3.org/2000/svg" {...props}>
    <path
      fillRule="evenodd"
      clipRule="evenodd"
      d="M8 5H9C9.55228 5 10 5.44772 10 6V11C10 11.5523 9.55229 12 9 12H1C0.447716 12 0 11.5523 0 11V6C0 5.44772 0.447715 5 1 5H2V3C2 1.34315 3.34315 0 5 0C6.65685 0 8 1.34315 8 3V5ZM7 5V3C7 1.89543 6.10457 1 5 1C3.89543 1 3 1.89543 3 3V5H7ZM3 6H9V11H1V6H2H3Z"
      className="fill-current"
      strokeMiterlimit="10"
    />
  </svg>
);
