import React, { Component } from 'react';
import _ from 'lodash';
import urbitOb from 'urbit-ob';
import Mousetrap from 'mousetrap';

import cn from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';
import { hexToRgba, uxToHex, deSig } from '/lib/util';

function ShipSearchItem({ ship, contacts, selected, onSelect }) {
  const contact = contacts[ship];
  let color = '#000000';
  let sigilClass = 'v-mid mix-blend-diff';
  let nickname;
  const nameStyle = {};
  const isSelected = ship === selected;
  if (contact) {
    const hex = uxToHex(contact.color);
    color = `#${hex}`;
    nameStyle.color = hexToRgba(hex, 0.7);
    nameStyle.textShadow = '0px 0px 0px #000';
    nameStyle.filter = 'contrast(1.3) saturate(1.5)';
    sigilClass = 'v-mid';
    nickname = contact.nickname;
  }

  return (
    <div
      onClick={() => onSelect(ship)}
      className={cn(
        'f8 pv1 ph3 pointer hover-bg-gray1-d hover-bg-gray4 relative flex items-center',
        {
          'white-d bg-gray0-d bg-white': !isSelected,
          'black-d bg-gray1-d bg-gray4': isSelected
        }
      )}
      key={ship}
    >
      <Sigil ship={'~' + ship} size={24} color={color} classes={sigilClass} />
      {nickname && (
        <p style={nameStyle} className="dib ml4 b">
          {nickname}
        </p>
      )}
      <div className="mono gray2 gray4-d ml4">{'~' + ship}</div>
      <p className="nowrap ml4">{status}</p>
    </div>
  );
}

export class ShipSearch extends Component {
  constructor() {
    super();

    this.state = {
      selected: null,
      suggestions: [],
      bound: false
    };

    this.keymap = {
        Tab: cm =>
          this.nextAutocompleteSuggestion(),
        'Shift-Tab': cm =>
          this.nextAutocompleteSuggestion(true),
        'Up': cm =>
          this.nextAutocompleteSuggestion(true),
        'Escape': cm =>
          this.props.onClear(),
        'Down': cm =>
          this.nextAutocompleteSuggestion(),
        'Enter': (cm) => {
          if(this.props.searchTerm !== null) {
            this.props.onSelect(this.state.selected);
          }
        },
        'Shift-3': cm =>
          this.toggleCode()
    };
  }

  componentDidMount() {
    if(this.props.searchTerm !== null) {
      this.updateSuggestions(true);
    }
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;

    if(!state.bound && props.inputRef) {
      this.bindShortcuts();
    }

    if(props.searchTerm === null) {
      if(state.suggestions.length > 0) {
        this.setState({ suggestions: [] });
      }
      this.unbindShortcuts();
      return;
    }

    if (
      props.searchTerm === null &&
      props.searchTerm !== prevProps.searchTerm &&
      props.searchTerm.startsWith(prevProps.searchTerm)
    ) {
      this.updateSuggestions();
    } else if (prevProps.searchTerm !== props.searchTerm) {
      this.updateSuggestions(true);
    }
  }

  updateSuggestions(isStale = false) {
    const needle = this.props.searchTerm;
    const matchString = (hay) => {
      hay = hay.toLowerCase();

      return (
        hay.startsWith(needle) ||
        _.some(_.words(hay), s => s.startsWith(needle))
      );
    };

    let candidates = this.state.suggestions;

    if (isStale || this.state.suggestions.length === 0) {
      const contacts = _.chain(this.props.contacts)
        .defaultTo({})
        .map((details, ship) => ({ ...details, ship }))
        .filter(
          ({ nickname, ship }) => matchString(nickname) || matchString(ship)
        )
        .map('ship')
        .value();

      const exactMatch = urbitOb.isValidPatp(`~${needle}`) ? [needle] : [];

      candidates = _.chain(this.props.candidates)
        .defaultTo([])
        .union(contacts)
        .union(exactMatch)
        .value();
    }

    const suggestions = _.chain(candidates)
      .filter(matchString)
      .filter(s => s.length < 28) // exclude comets
      .value();

    this.bindShortcuts();
    this.setState({ suggestions, selected: suggestions[0] });
  }

  bindCmShortcuts()  {
    if(!this.props.cm) {
      return;
    }
    this.props.cm.addKeyMap(this.keymap);
  }

  unbindCmShortcuts() {
    if(!this.props.cm) {
      return;
    }
    this.props.cm.removeKeyMap(this.keymap);
  }

  bindShortcuts() {
    if (this.state.bound) {
      return;
    }
    if (!this.props.inputRef) {
      return this.bindCmShortcuts();
    }
    this.setState({ bound: true });
    if (!this.mousetrap) {
      this.mousetrap = new Mousetrap(this.props.inputRef);
    }

    this.mousetrap.bind('enter', (e) => {
      e.preventDefault();
      e.stopPropagation();

      if (this.state.selected) {
        this.unbindShortcuts();
        this.props.onSelect(this.state.selected);
      }
    });

    this.mousetrap.bind('tab', (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.nextAutocompleteSuggestion(false);
    });
    this.mousetrap.bind(['up', 'shift+tab'], (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.nextAutocompleteSuggestion(true);
    });
    this.mousetrap.bind('down', (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.nextAutocompleteSuggestion(false);
    });
    this.mousetrap.bind('esc', (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.props.onClear();
    });
  }

  unbindShortcuts() {
    if(!this.props.inputRef) {
      this.unbindCmShortcuts();
    }

    if (!this.state.bound) {
      return;
    }

    this.setState({ bound: false });
    this.mousetrap.unbind('enter');
    this.mousetrap.unbind('tab');
    this.mousetrap.unbind(['up', 'shift+tab']);
    this.mousetrap.unbind('down');
    this.mousetrap.unbind('esc');
  }

  nextAutocompleteSuggestion(backward = false) {
    const { suggestions } = this.state;
    let idx = suggestions.findIndex(s => s === this.state.selected);

    idx = backward ? idx - 1 : idx + 1;
    idx = idx % suggestions.length;
    if (idx < 0) {
      idx = suggestions.length - 1;
    }

    this.setState({ selected: suggestions[idx] });
  }

  render() {
    const { onSelect, contacts, popover, className } = this.props;
    const { selected, suggestions } = this.state;

    if (suggestions.length === 0) {
      return null;
    }

    const popoverClasses = (popover && ' absolute ') || ' ';
    return (
      <div
        style={
          popover
            ? {
                bottom: '90%',
                left: '48px'
              }
            : {}
        }
        className={
          'black white-d bg-white bg-gray0-d ' +
            'w7 pv3 z-1 mt1 ba b--gray1-d b--gray4' +
            popoverClasses +
            className || ''
        }
      >
        {suggestions.slice(0, 5).map(ship => (
          <ShipSearchItem
            onSelect={onSelect}
            key={ship}
            selected={selected}
            contacts={contacts}
            ship={ship}
          />
        ))}
      </div>
    );
  }
}

export class ShipSearchInput extends Component {
  constructor() {
    super();
    this.state = {
      searchTerm: ''
    };

    this.inputRef = null;
    this.popoverRef = null;

    this.search = this.search.bind(this);

    this.onClick = this.onClick.bind(this);
    this.setInputRef = this.setInputRef.bind(this);
  }

  onClick(event) {
    const { popoverRef } = this;
    // Do nothing if clicking ref's element or descendent elements
    if (!popoverRef || popoverRef.contains(event.target)) {
      return;
    }

    this.props.onClear();
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.onClick);
    document.addEventListener('touchstart', this.onClick);
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.onClick);
    document.removeEventListener('touchstart', this.onClick);
  }

  setInputRef(ref) {
    this.inputRef = ref;
    if(ref) {
      ref.focus();
    }
    // update this.inputRef prop
    this.forceUpdate();
  }

  search(e) {
    const searchTerm = e.target.value;
    this.setState({ searchTerm });
  }

  render() {
    const { state, props } = this;

    return (
      <div
        ref={ref => (this.popoverRef = ref)}
        style={{ top: '150%', left: '-80px' }}
        className="b--gray2 b--solid ba absolute bg-white bg-gray0-d"
      >
        <textarea
          style={{ resize: 'none', maxWidth: '200px' }}
          className="ma2 pa2 b--gray4 ba b--solid w7 db bg-gray0-d white-d"
          rows={1}
          autocapitalise="none"
          autoFocus={
            /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
              navigator.userAgent
            )
              ? false
              : true
          }
          placeholder="Search for a ship"
          value={state.searchTerm}
          onChange={this.search}
          ref={this.setInputRef}
        />
        <ShipSearch
          contacts={props.contacts}
          candidates={props.candidates}
          searchTerm={deSig(state.searchTerm)}
          inputRef={this.inputRef}
          onSelect={props.onSelect}
          onClear={props.onClear}
        />
      </div>
    );
  }
}
