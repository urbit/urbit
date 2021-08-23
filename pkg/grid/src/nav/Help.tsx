import React, { useEffect } from 'react';
import { useLeapStore } from './Nav';

export const Help = () => {
  const select = useLeapStore((state) => state.select);

  useEffect(() => {
    select('Help and Support');
  }, []);

  return (
    <div className="p-4 md:p-8 space-y-8">
      <h2 className="h4 text-gray-500">Recent Apps</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
      <hr className="-mx-4 md:-mx-8" />
      <h2 className="h4 text-gray-500">Recent Developers</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
    </div>
  );
};
