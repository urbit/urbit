import React from 'react';

export default function PencilIcon(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg {...props} viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M15.707 4.707a1 1 0 1 0-1.414-1.414l-9 9a1 1 0 0 0-.255.432l-2 7a1 1 0 0 0 1.237 1.236l7-2a.999.999 0 0 0 .432-.254l9-9a1 1 0 1 0-1.414-1.414l-8.817 8.817-3.04.868 13.271-13.27a1 1 0 1 0-1.414-1.415L6.022 16.564l.868-3.04 8.817-8.817ZM15 19a1 1 0 1 0 0 2h5a1 1 0 1 0 0-2h-5Z"
        className="fill-current"
      />
    </svg>
  );
}
