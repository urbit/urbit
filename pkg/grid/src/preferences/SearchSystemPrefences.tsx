import React, { ChangeEvent, KeyboardEvent, useEffect, useState } from 'react';
import { Link, useHistory } from 'react-router-dom';
import fuzzy from 'fuzzy';
import classNames from 'classnames';
import MagnifyingGlassIcon from '../components/icons/MagnifyingGlassIcon';
import BellIcon from '../components/icons/BellIcon';
import { Interface } from '../components/icons/Interface';
import BurstIcon from '../components/icons/BurstIcon';
import HelpIcon from '../components/icons/HelpIcon';
import TlonIcon from '../components/icons/TlonIcon';
import LogoutIcon from '../components/icons/LogoutIcon';
import PencilIcon from '../components/icons/PencilIcon';
import ForwardSlashIcon from '../components/icons/ForwardSlashIcon';

const navOptions: { route: string; title: string; icon: React.ReactElement }[] = [
  {
    route: 'help',
    title: 'Help and Support',
    icon: <HelpIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'interface',
    title: 'Interface Settings',
    icon: <Interface className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'notifications',
    title: 'Notifications',
    icon: <BellIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'appearance',
    title: 'Appearance',
    icon: <PencilIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'shortcuts',
    title: 'Shortcuts',
    icon: <ForwardSlashIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'privacy',
    title: 'Attention & Privacy',
    icon: <BurstIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'security',
    title: 'Log Out...',
    icon: <LogoutIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'system-updates',
    title: 'About System',
    icon: <TlonIcon className="w-4 h-4 text-gray-600" />
  }
];

interface SearchSystemPrefencesProps {
  subUrl: (submenu: string) => string;
}

export default function SearchSystemPreferences({ subUrl }: SearchSystemPrefencesProps) {
  const { push } = useHistory();
  const [searchInput, setSearchInput] = useState('');
  const [matchingNavOptions, setMatchingNavOptions] = useState<string[]>([]);
  const [highlightNavOption, setHighlightNavOption] = useState<number>();

  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const input = e.target as HTMLInputElement;
    const value = input.value.trim();

    setSearchInput(value);
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    const { key } = e;
    if (key === 'ArrowDown' && searchInput !== '' && matchingNavOptions.length > 0) {
      if (highlightNavOption === undefined) {
        setHighlightNavOption(0);
      } else {
        setHighlightNavOption((prevState) => prevState! + 1);
      }
    }

    if (
      key === 'ArrowUp' &&
      searchInput !== '' &&
      matchingNavOptions.length > 0 &&
      highlightNavOption !== undefined &&
      highlightNavOption !== 0
    ) {
      setHighlightNavOption((prevState) => prevState! - 1);
    }

    if (key === 'Enter' && searchInput !== '' && highlightNavOption !== undefined) {
      push(subUrl(navOptions[highlightNavOption].route));
    }
  };

  const handleBlur = () => {
    setSearchInput('');
  };

  useEffect(() => {
    const results = fuzzy.filter(searchInput, navOptions, { extract: (obj) => obj.title });
    const matches = results.map((el) => el.string);
    setMatchingNavOptions(matches);
  }, [searchInput]);

  return (
    <>
      <label className="relative flex items-center">
        <span className="sr-only">Search Prefences</span>
        <span className="absolute h-8 w-8 text-gray-400 flex items-center pl-2 inset-y-1 left-0">
          <MagnifyingGlassIcon />
        </span>
        <input
          className="input bg-gray-50 pl-8 placeholder:font-semibold mb-5 h-10"
          placeholder="Search Preferences"
          value={searchInput}
          onChange={handleChange}
          onKeyDown={handleKeyDown}
          onBlur={handleBlur}
        />
      </label>
      <div className="relative">
        {matchingNavOptions.length > 0 && searchInput !== '' ? (
          <div className="absolute -top-3 flex flex-col bg-white space-y-2 rounded-2xl shadow-md w-full py-3">
            {matchingNavOptions.map((opt, index) => {
              const matchingNavOption = navOptions.find((navOpt) => navOpt.title === opt);
              if (matchingNavOption !== undefined) {
                return (
                  <Link
                    className={classNames(
                      'flex px-2 py-3 items-center space-x-2 hover:text-black hover:bg-gray-50',
                      {
                        'bg-gray-50': highlightNavOption === index
                      }
                    )}
                    to={subUrl(matchingNavOption.route)}
                  >
                    {matchingNavOption.icon}
                    <span className="text-gray-900">{matchingNavOption?.title}</span>
                  </Link>
                );
              }
              return null;
            })}
          </div>
        ) : null}
      </div>
    </>
  );
}
