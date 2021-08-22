import React from 'react';

export const Home = () => {
  return (
    <div className="h-full p-4 md:p-8 space-y-8 overflow-y-auto">
      <h2 className="h4 text-gray-500">Recent Apps</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
      <hr className="-mx-4 md:-mx-8" />
      <h2 className="h4 text-gray-500">Recent Developers</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
    </div>
  );
};
