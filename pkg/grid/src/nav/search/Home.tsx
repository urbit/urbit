import { debounce } from 'lodash-es';
import React, { useCallback, useEffect } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { createNextPath, useNavStore } from '../Nav';

type HomeProps = RouteComponentProps;

export const Home = ({ match, history }: HomeProps) => {
  const searchInput = useNavStore((state) => state.searchInput);
  const { push } = history;
  const { path } = match;

  const handleSearch = useCallback(
    debounce((input: string) => {
      push(createNextPath(path, input.trim()));
    }, 300),
    [path]
  );

  useEffect(() => {
    if (searchInput) {
      handleSearch(searchInput);
    }
  }, [searchInput]);

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
