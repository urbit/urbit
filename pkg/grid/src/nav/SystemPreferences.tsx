import React, { useEffect } from 'react';
import { useNavStore } from './Nav';

export const SystemPreferences = () => {
  const select = useNavStore((state) => state.select);

  useEffect(() => {
    select('System Preferences');
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
