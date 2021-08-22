import React from 'react';

export const SpinnerIcon = (props: React.SVGProps<SVGSVGElement>) => (
  <svg {...props} fill="none" viewBox="0 0 24 24">
    <circle cx="12" cy="12" r="7" className="stroke-current" strokeOpacity="0.4" strokeWidth="8" />
    <path
      className="fill-current"
      d="M23 12C23 13.7359 22.5892 15.4472 21.8011 16.9939C21.013 18.5406 19.87 19.8788 18.4656 20.8992C17.0613 21.9195 15.4353 22.593 13.7208 22.8646C12.0062 23.1361 10.2518 22.998 8.60081 22.4616L11.0482 14.9293C11.5105 15.0795 12.0017 15.1181 12.4818 15.0421C12.9619 14.966 13.4172 14.7775 13.8104 14.4918C14.2036 14.2061 14.5236 13.8314 14.7443 13.3983C14.965 12.9652 15.08 12.4861 15.08 12L23 12Z"
    />
  </svg>
);
