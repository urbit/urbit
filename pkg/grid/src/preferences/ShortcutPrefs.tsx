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
    keybinding: 'Alt + X'
  },
  { action: 'Down in List', keybinding: 'Alt + X' },
  { action: 'Next Page', keybinding: 'Alt + X' },
  { action: 'Previous Page', keybinding: 'Alt + X' },
  { action: 'Context-Aware Search', keybinding: 'Ctrl + /' }
];

interface SearchKeyboardShortcutsProps {
  searchInput: string;
  setSearchInput: (input: string) => void;
  setMatchingShortcuts: (newMatchingShortcuts: string[]) => void;
}

const SearchKeyboardShortcuts = ({
  searchInput,
  setSearchInput,
  setMatchingShortcuts
}: SearchKeyboardShortcutsProps) => {
  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const input = e.target as HTMLInputElement;
    const { value } = input;

    setSearchInput(value);
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
          onBlur={handleBlur}
        />
      </label>
    </>
  );
};

export const ShortcutPrefs = () => {
  const [searchInput, setSearchInput] = useState('');
  const [matchingShortcuts, setMatchingShortcuts] = useState<string[]>([]);
  const metaKey = window.navigator.platform.includes('Mac') ? '⌘' : 'Ctrl';
  const altKey = window.navigator.platform.includes('Mac') ? '⌥' : 'Alt';

  return (
    <div className="inner-section space-y-8">
      <h2 className="h4">Keyboard Shortcuts</h2>
      <SearchKeyboardShortcuts
        searchInput={searchInput}
        setSearchInput={setSearchInput}
        setMatchingShortcuts={setMatchingShortcuts}
      />
      <div className="grid grid-cols-2 rounded-lg border-2 border-gray-50 bg-gray-50 gap-y-2">
        <span className="px-3 py-2 text-gray-400 text-sm font-semibold">Action</span>
        <span className="px-3 py-2 text-gray-400 text-sm font-semibold">Keybinding</span>
        {shortcuts
          .map((shortcut) => ({
            action: shortcut.action,
            keybinding: shortcut.keybinding.replace('Ctrl', metaKey).replace('Alt', altKey)
          }))
          .filter((shortcut) =>
            matchingShortcuts.length > 0
              ? matchingShortcuts.find((sc) => shortcut.action === sc)
              : true
          )
          .map((shortcut, index) => (
            <React.Fragment key={`${shortcut.action}-${index}`}>
              <span
                className={classNames('text-gray-800 font-semibold p-3 rounded-lg', {
                  'bg-white': index % 2 === 0
                })}
              >
                {shortcut.action}
              </span>
              <span
                className={classNames('text-gray-800 font-semibold p-3 rounded-lg', {
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
