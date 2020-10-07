import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { Box, Row, Rule, Text } from '@tlon/indigo-react';
import index from '~/logic/lib/omnibox';
import Mousetrap from 'mousetrap';
import OmniboxInput from './OmniboxInput';
import OmniboxResult from './OmniboxResult';

import defaultApps from '~/logic/lib/default-apps';

export class Omnibox extends Component {
  constructor(props) {
    super(props);
    this.state = {
      index: new Map([]),
      query: '',
      results: this.initialResults(),
      selected: []
    };
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.search = this.search.bind(this);
    this.navigate = this.navigate.bind(this);
    this.control - this.control.bind(this);
    this.setPreviousSelected = this.setPreviousSelected.bind(this);
    this.setNextSelected = this.setNextSelected.bind(this);
    this.renderResults = this.renderResults.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    if (prevProps !== this.props) {
      const { pathname } = this.props.location;
      const selectedGroup = pathname.startsWith('/~landscape/ship/') ? '/' + pathname.split('/').slice(2,5).join('/') : null;

      this.setState({ index: index(this.props.associations, this.props.apps.tiles, selectedGroup) });
    }

    if (prevProps && (prevProps.apps !== this.props.apps) && (this.state.query === '')) {
      this.setState({ results: this.initialResults() });
    }

    if (prevProps && this.props.show && prevProps.show !== this.props.show) {
      Mousetrap.bind('escape', () => this.props.api.local.setOmnibox());
      document.addEventListener('mousedown', this.handleClickOutside);
      const touchstart = new Event('touchstart');
      this.omniInput.input.dispatchEvent(touchstart);
      this.omniInput.input.focus();
    }
  }

  componentWillUpdate(prevProps) {
    if (this.props.show && prevProps.show !== this.props.show) {
      Mousetrap.unbind('escape');
      document.removeEventListener('mousedown', this.handleClickOutside);
    }
  }

  getSearchedCategories() {
    return ['commands', 'groups', 'subscriptions', 'other'];
  }

  control(evt) {
    if (evt.key === 'Escape') {
      if (this.state.query.length > 0) {
        this.setState({ query: '', results: this.initialResults(), selected: [] });
      } else if (this.props.show) {
        this.props.api.local.setOmnibox();
      }
    };

    if (
      evt.key === 'ArrowUp' ||
      (evt.shiftKey && evt.key === 'Tab')) {
        evt.preventDefault();
      return this.setPreviousSelected();
    }

    if (evt.key === 'ArrowDown' || evt.key === 'Tab') {
      evt.preventDefault();
      this.setNextSelected();
    }

    if (evt.key === 'Enter') {
      evt.preventDefault();
      if (this.state.selected.length > 0) {
        this.navigate(this.state.selected[0], this.state.selected[1]);
      } else if (Array.from(this.state.results.values()).flat().length === 0) {
        return;
      } else {
        this.navigate(
          Array.from(this.state.results.values()).flat()[0].app,
          Array.from(this.state.results.values()).flat()[0].link);
      }
    }
  }

  handleClickOutside(evt) {
    if (this.props.show && !this.omniBox.contains(evt.target)) {
      this.setState({ results: this.initialResults(), query: '', selected: [] }, () => {
        this.props.api.local.setOmnibox();
      });
    }
  }

  initialResults() {
    return new Map(this.getSearchedCategories().map((category) => {
      if (!this.state) {
        return [category, []];
      }
      if (category === 'apps') {
        return ['apps', this.state.index.get('apps')];
      }
      if (category === 'other') {
        return ['other', this.state.index.get('other')];
      }
      return [category, []];
    }));
  }

  navigate(app, link) {
    const { props } = this;
    this.setState({ results: this.initialResults(), query: '' }, () => {
      props.api.local.setOmnibox();
      if (defaultApps.includes(app.toLowerCase()) || app === 'profile' || app === 'Links') {
        props.history.push(link);
      } else {
        window.location.href = link;
      }
    });
  }

  search(event) {
    const { state } = this;
    let query = event.target.value;
    const results = this.initialResults();
    let selected = state.selected;

    this.setState({ query });

    // wipe results if backspacing
    if (query.length === 0) {
      this.setState({ results: results, selected: [] });
      return;
    }

    // don't search for single characters
    if (query.length === 1) {
      return;
    }

    query = query.toLowerCase();

    this.getSearchedCategories().map((category) => {
      const categoryIndex = state.index.get(category);
      results.set(category,
        categoryIndex.filter((result) => {
          return (
            result.title.toLowerCase().includes(query) ||
            result.link.toLowerCase().includes(query) ||
            result.app.toLowerCase().includes(query) ||
            (result.host !== null ? result.host.includes(query) : false)
          );
        })
      );
    });

    const flattenedResultLinks = Array.from(results.values()).flat().map(result => [result.app, result.link]);
    if (!flattenedResultLinks.includes(selected)) {
      selected = flattenedResultLinks[0] || [];
    }

    this.setState({ results, selected });
  }

  setPreviousSelected() {
    const current = this.state.selected;
    const flattenedResults = Array.from(this.state.results.values()).flat();
    const totalLength = flattenedResults.length;
    if (current !== []) {
      const currentIndex = flattenedResults.indexOf(
        ...flattenedResults.filter((e) => {
          return e.link === current[1];
        })
      );
      if (currentIndex > 0) {
        const { app, link } = flattenedResults[currentIndex - 1];
        this.setState({ selected: [app, link] });
      } else {
        const { app, link } = flattenedResults[totalLength - 1];
        this.setState({ selected: [app, link] });
      }
    } else {
      const { app, link } = flattenedResults[totalLength - 1];
      this.setState({ selected: [app, link] });
    }
  }

  setNextSelected() {
    const current = this.state.selected;
    const flattenedResults = Array.from(this.state.results.values()).flat();
    if (current !== []) {
      const currentIndex = flattenedResults.indexOf(
        ...flattenedResults.filter((e) => {
          return e.link === current[1];
        })
      );
      if (currentIndex < flattenedResults.length - 1) {
        const { app, link } = flattenedResults[currentIndex + 1];
        this.setState({ selected: [app, link] });
      } else {
        const { app, link } = flattenedResults[0];
        this.setState({ selected: [app, link] });
      }
    } else {
      const { app, link } = flattenedResults[0];
      this.setState({ selected: [app, link] });
    }
  }

  renderResults() {
    const { props, state } = this;
    return <Box
            maxHeight="400px"
            overflowY="auto"
            overflowX="hidden"
            borderBottomLeftRadius='2'
            borderBottomRightRadius='2'
           >
      {this.getSearchedCategories()
        .map(category => Object({ category, categoryResults: state.results.get(category) }))
        .filter(category => category.categoryResults.length > 0)
        .map(({ category, categoryResults }, i) => {
          const categoryTitle = (category === 'other')
            ? null : <Text gray ml={2}>{category.charAt(0).toUpperCase() + category.slice(1)}</Text>;
          const selected = this.state.selected?.length ? this.state.selected[1] : '';
          return (<Box key={i} width='max(50vw, 300px)' maxWidth='600px'>
            <Rule borderTopWidth="0.5px" color="washedGray" />
            {categoryTitle}
            {categoryResults.map((result, i2) => (
              <OmniboxResult
                key={i2}
                icon={result.app}
                text={result.title}
                subtext={result.host}
                link={result.link}
                navigate={() => this.navigate(result.app, result.link)}
                selected={selected}
                dark={props.dark} />
            ))}
          </Box>
        );
      })
      }
    </Box>;
  }

  render() {
    const { props, state } = this;
    if (state?.selected?.length === 0 && Array.from(this.state.results.values()).flat().length) {
      this.setNextSelected();
    }
    return (
        <Box
          backgroundColor='scales.black30'
          width='100%'
          height='100%'
          position='absolute'
          top='0'
          right='0'
          zIndex='9'
          display={props.show ? 'block' : 'none'}>
          <Row justifyContent='center'>
            <Box
              mt='20vh'
              width='max(50vw, 300px)'
              maxWidth='600px'
              borderRadius='2'
              backgroundColor='white'
              ref={(el) => {
                this.omniBox = el;
              }}>
              <OmniboxInput
                ref={(el) => {
                  this.omniInput = el;
                }}
                control={e => this.control(e)}
                search={this.search}
                query={state.query}
              />
              {this.renderResults()}
            </Box>
          </Row>
        </Box>
    );
  }
}

export default withRouter(Omnibox);
