import React from 'react';
import classNames from 'classnames';

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

export const ShortcutPrefs = () => {
  return (
    <div className="inner-section space-y-8">
      <h2 className="h4">Keyboard Shortcuts</h2>
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
