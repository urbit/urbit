import React, { ChangeEvent, KeyboardEvent, useEffect, useState } from 'react';
import classNames from 'classnames';
import fuzzy from 'fuzzy';
import MagnifyingGlassIcon from '../components/icons/MagnifyingGlassIcon';

interface Shortcut {
  action: string;
  keybinding: string;
}

const shortcuts: Shortcut[] = [
  {
    action: 'Up in List',
    keybinding: 'Opt + X'
  },
  { action: 'Down in List', keybinding: 'Opt + X' },
  { action: 'Next Page', keybinding: 'Opt + X' },
  { action: 'Previous Page', keybinding: 'Opt + X' },
  { action: 'Context-Aware Search', keybinding: 'Ctrl + /' }
];

const SearchKeyboardShortcuts = () => {
  const [searchInput, setSearchInput] = useState('');
  const [matchingShortcuts, setMatchingShortcuts] = useState<string[]>([]);
  const [highlightShortcut, setHighlightShortcut] = useState<number>();

  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const input = e.target as HTMLInputElement;
    const value = input.value.trim();

    setSearchInput(value);
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    const { key } = e;
    if (key === 'ArrowDown' && searchInput !== '' && matchingShortcuts.length > 0) {
      if (highlightShortcut === undefined) {
        setHighlightShortcut(0);
      } else {
        setHighlightShortcut((prevState) => prevState! + 1);
      }
    }

    if (
      key === 'ArrowUp' &&
      searchInput !== '' &&
      matchingShortcuts.length > 0 &&
      highlightShortcut !== undefined &&
      highlightShortcut !== 0
    ) {
      setHighlightShortcut((prevState) => prevState! - 1);
    }

    if (key === 'Enter' && searchInput !== '' && highlightShortcut !== undefined) {
      // push(subUrl(navOptions[highlightShortcut].route));
    }
  };

  const handleBlur = () => {
    setSearchInput('');
  };

  useEffect(() => {
    const results = fuzzy.filter(searchInput, shortcuts, { extract: (obj) => obj.action });
    const matches = results.map((el) => el.string);
    setMatchingShortcuts(matches);
  }, [searchInput]);

  return (
    <>
      <label className="relative flex items-center">
        <span className="sr-only">Search Actions</span>
        <span className="absolute h-8 w-8 text-gray-400 flex items-center pl-2 inset-y-1 left-0">
          <MagnifyingGlassIcon />
        </span>
        <input
          className="input bg-gray-50 pl-8 placeholder:font-semibold mb-5 h-10"
          placeholder="Search Actions"
          value={searchInput}
          onChange={handleChange}
          onKeyDown={handleKeyDown}
          onBlur={handleBlur}
        />
      </label>
      <div className="relative">
        {matchingShortcuts.length > 0 && searchInput !== '' ? (
          <div className="absolute -top-12 flex flex-col bg-white space-y-2 rounded-2xl shadow-md w-full py-3">
            {matchingShortcuts.map((opt, index) => {
              const matchingShortcut = shortcuts.find((shortcut) => shortcut.action === opt);
              if (matchingShortcut !== undefined) {
                return (
                  <div
                    key={`${opt}-${index}`}
                    className={classNames(
                      'flex px-2 py-3 items-center space-x-2 hover:text-black hover:bg-gray-50',
                      {
                        'bg-gray-50': highlightShortcut === index
                      }
                    )}
                    // to={subUrl(matchingNavOption.route)}
                  >
                    <span className="text-gray-900">{matchingShortcut?.action}</span>
                  </div>
                );
              }
              return null;
            })}
          </div>
        ) : null}
      </div>
    </>
  );
};

export const ShortcutPrefs = () => {
  return (
    <div className="inner-section space-y-8">
      <h2 className="h4">Keyboard Shortcuts</h2>
      <SearchKeyboardShortcuts />
      <div className="grid grid-cols-2 rounded-lg border-2 border-gray-50 bg-gray-50 gap-y-2">
        <span className="px-3 py-2 text-gray-400 text-sm font-semibold">Action</span>
        <span className="px-3 py-2 text-gray-400 text-sm font-semibold">Keybinding</span>
        {shortcuts.map((shortcut, index) => (
          <React.Fragment key={`${shortcut.action}-${index}`}>
            <span
              className={classNames('text-gray-800 font-semibold p-3', {
                'bg-white': index % 2 === 0
              })}
            >
              {shortcut.action}
            </span>
            <span
              className={classNames('text-gray-800 font-semibold p-3', {
                'bg-white': index % 2 === 0
              })}
            >
              {shortcut.keybinding}
            </span>
          </React.Fragment>
        ))}
      </div>
    </div>
  );
};
