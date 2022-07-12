import React from 'react';

export default function LogoutIcon(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg {...props} viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M7.414 11H14a1 1 0 1 1 0 2H7.414l2.293 2.293a1 1 0 0 1-1.414 1.414l-4-4a1 1 0 0 1 0-1.414l4-4a1 1 0 0 1 1.414 1.414L7.414 11ZM14 18a1 1 0 1 0 0 2h3a3 3 0 0 0 3-3V7a3 3 0 0 0-3-3h-3a1 1 0 1 0 0 2h3a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1h-3Z"
        className="fill-current"
      />
    </svg>
  );
}
