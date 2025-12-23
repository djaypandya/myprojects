import type { Metadata } from 'next'
import { Lora, Caveat } from 'next/font/google'
import './globals.css'
import Navigation from '@/components/Navigation'

const lora = Lora({
  subsets: ['latin'],
  variable: '--font-lora',
  display: 'swap',
})

const caveat = Caveat({
  subsets: ['latin'],
  variable: '--font-hand',
  display: 'swap',
})

export const metadata: Metadata = {
  title: 'Erandi Studio',
  description: 'Luxury Interior Design Studio',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={`${lora.variable} ${caveat.variable}`}>
        <Navigation />
        {children}
      </body>
    </html>
  )
}
