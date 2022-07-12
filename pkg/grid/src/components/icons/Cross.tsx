import React from 'react';

export const Cross = (props: React.SVGProps<SVGSVGElement>) => (
  <svg {...props} viewBox="0 0 12 12">
    <path
      fillRule="evenodd"
      clipRule="evenodd"
      d="M0.292893 10.2942C-0.0976311 10.6847 -0.0976311 11.3179 0.292893 11.7084C0.683417 12.0989 1.31658 12.0989 1.70711 11.7084L6.00063 7.41487L10.2948 11.7091C10.6853 12.0996 11.3185 12.0996 11.709 11.7091C12.0996 11.3185 12.0996 10.6854 11.709 10.2949L7.41484 6.00066L11.7084 1.70711C12.0989 1.31658 12.0989 0.683417 11.7084 0.292894C11.3179 -0.0976312 10.6847 -0.0976312 10.2942 0.292894L6.00063 4.58645L1.70775 0.293571C1.31723 -0.0969534 0.684061 -0.0969534 0.293536 0.293571C-0.0969879 0.684095 -0.0969879 1.31726 0.293536 1.70778L4.58641 6.00066L0.292893 10.2942Z"
      className="fill-current"
    />
  </svg>
);
