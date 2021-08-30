import React, { useEffect } from 'react';
import { useLeapStore } from './Nav';
import helpAndSupport from '../assets/help-and-support.svg';

export const Help = () => {
  const select = useLeapStore((state) => state.select);

  useEffect(() => {
    select('Help and Support');
  }, []);

  return (
    <div className="flex flex-col items-center px-4 py-8 md:px-8 md:py-16 space-y-8 md:space-y-16">
      <img className="w-52 h-auto" src={helpAndSupport} alt="" />
      <div className="h4 space-y-4 md:space-y-8">
        <div>
          <h2>User Help Docs</h2>
          <a
            className="text-blue-400"
            href="mailto:support@tlon.io"
            target="_blank"
            rel="noreferrer"
          >
            tlon.io/help
          </a>
        </div>
        <div>
          <h2>For general help, ask the community</h2>
          <a
            className="text-blue-400"
            href="web+urbitgraph://group/~bitbet-bolbel/urbit-community"
            target="_blank"
            rel="noreferrer"
          >
            Join Urbit Community
          </a>
        </div>
        <div>
          <h2>For all other issues:</h2>
          <a
            className="text-blue-400"
            href="mailto:support@tlon.io"
            target="_blank"
            rel="noreferrer"
          >
            support@tlon.io
          </a>
        </div>
      </div>
    </div>
  );
};
