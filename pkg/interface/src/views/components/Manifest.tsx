import { string } from 'prop-types';
import React, { Component } from 'react';
import { manifestUrl } from '~/logic/lib/util';

interface PWAIcon {
  src: string;
  sizes: string;
  type: string;
  purpose?: string;
}

interface PWARelatedApplication {
  platform?: 'chrome_web_store' | 'play' | 'windows' | 'itunes';
  url?: string;
  id?: string;
}

interface PWAShortcut {
  name: string;
  short_name?: string;
  description?: string;
  url: string;
  icons?: PWAIcon[];
}

interface WebAppManifest {
  background_color?: string;
  categories?: string[];
  description?: string;
  dir?: 'auto' | 'ltr' | 'rtl';
  display?: 'fullscreen' | 'standalone' | 'minimal-ui' | 'browser';
  iarc_rating_id?: string;
  icons?: PWAIcon[];
  lang?: string;
  name: string;
  orientation?: 'any' | 'natural' | 'landscape' | 'landscape-primary' | 'landscape-secondary' | 'portrait' | 'portrait-primary' | 'portrait-secondary';
  prefer_related_applications?: boolean;
  related_applications?: PWARelatedApplication[];
  scope?: string;
  screenshots?: PWAIcon[]; 
  short_name?: string;
  shortcuts?: PWAShortcut[];
  start_url?: string;
  theme_color?: string;
}

interface ManifestProps {
  data: WebAppManifest;
}



// Basically the pattern of this component is:
// 1. Wrapper renders default manifest
// 2. Child receives new information,
// 3. Child gets current information
// 4. Overwrites current information where specified
// 5. Generates new blob and replaces link[rel=manifest].href
// 6. Hopefully no one gets in each other's way
interface ManifestState {}

export default class Manifest extends Component<ManifestProps, ManifestState> {
  constructor(props) {
    super(props);
    this.state = {
      content: {}
    };
  }

  componentDidMount() {
    console.log('blob2', this.props);
    const oldLink = document.querySelector('link[rel=manifest]');
    console.log('blobold', oldLink);
    if (!oldLink) {
      const link = document.createElement('link');
      link.rel = 'manifest';
      link.href = manifestUrl(this.props.data);
      document.head.appendChild(link);
    } else {
      const url = oldLink.href;
      fetch(url).then(r => r.text()).then(json => {
        const oldManifest = JSON.parse(json);
        const newManifest = this.props.data;
        Object.keys(oldManifest).forEach(key => {
          if (!newManifest[key]) {
            newManifest[key] = oldManifest[key];
          }
        });
        oldLink.href = manifestUrl(newManifest);
      });
    }
  }

  render() {

    // Array.from(document.querySelectorAll('link[rel=manifest]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));
    Array.from(document.querySelectorAll('link[rel="apple-touch-icon"]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));
    Array.from(document.querySelectorAll('meta[name="apple-mobile-web-app-status-bar-style"]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));

    const { data } = this.props;
    return null;
  }
}