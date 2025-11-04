declare module 'react-plotly.js' {
  import { Component } from 'react'
  import { Data, Layout, Config } from 'plotly.js'

  interface PlotParams {
    data: Data[]
    layout?: Partial<Layout> & { template?: string | any }
    config?: Partial<Config>
    revision?: number
    onInitialized?: (figure: any, graphDiv: HTMLElement) => void
    onUpdate?: (figure: any, graphDiv: HTMLElement) => void
    onPurge?: (figure: any, graphDiv: HTMLElement) => void
    onError?: (err: any) => void
    debug?: boolean
    useResizeHandler?: boolean
    style?: React.CSSProperties
    className?: string
  }

  export default class Plot extends Component<PlotParams> {}
}

