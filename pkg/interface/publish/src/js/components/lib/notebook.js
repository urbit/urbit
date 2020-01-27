import React, { Component } from 'react';
import { NoteList } from './note-list';
import { About } from './about';
import { Subscribers } from './subscribers';
import { Settings } from './settings';

//TODO subcomponents for posts, subscribers, settings
//
//TODO props.view switch for which component to render
//pass props.notebook, contacts to each component

//TODO ask for notebook if we don't have it
//
//TODO initialise notebook obj if no props.notebook

//TODO component bar above the rendered component
//don't render settings if it's ours
//current component is black, others gray2 (see Chat's tab bar for an example)

export class Notebook extends Component {
  render() {
    return (
      <div>

      </div>
    )
  }
}

export default Notebook
