import React from 'react';

export default function BurstIcon(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg {...props} viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M13 4a1 1 0 1 0-2 0v2a1 1 0 1 0 2 0V4Zm0 14a1 1 0 1 0-2 0v2a1 1 0 1 0 2 0v-2Zm8-6a1 1 0 0 1-1 1h-2a1 1 0 1 1 0-2h2a1 1 0 0 1 1 1ZM6 13a1 1 0 1 0 0-2H4a1 1 0 1 0 0 2h2Zm-.364-7.364a1 1 0 0 1 1.414 0L8.464 7.05A1 1 0 0 1 7.05 8.464L5.636 7.05a1 1 0 0 1 0-1.414Zm11.314 9.9a1 1 0 0 0-1.414 1.414l1.414 1.414a1 1 0 0 0 1.414-1.414l-1.414-1.414Zm1.414-9.9a1 1 0 0 1 0 1.414L16.95 8.464a1 1 0 0 1-1.414-1.414l1.414-1.414a1 1 0 0 1 1.414 0Zm-9.9 11.314a1 1 0 0 0-1.414-1.414L5.636 16.95a1 1 0 1 0 1.414 1.414l1.414-1.414Z"
        className="fill-current"
      />
    </svg>
  );
}
